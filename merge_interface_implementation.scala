import java.io.File
import java.io.PrintWriter
import scala.io.Source._

object OCamlInterfaceMerger {

  /** Merges the interface and the implementation of an OCaml file.
    *
    * @param args the command line arguments containing the name of 3 files:
    *             1. The file containing the interface.
    *             2. The file containing the implementation.
    *             3. The file that will contain the interface and implementation merged.
    */
  def main(args: Array[String]): Unit = {
    if (args.length < 3)
      throw new IllegalArgumentException("Not enough arguments were given.")

    val interfaceFile = args(0)
    val implementationFile = args(1)
    val outputFile = args(2)

    val typesMap = parseInterface(interfaceFile)
    writeMergedFile(typesMap, implementationFile, outputFile)
  }

  def parseInterface(fileName: String): Map[String, List[String]] = {
    val file = fromFile(fileName)

    def processLine(
        lines: List[String],
        typesMapAcc: Map[String, List[String]]
    ): Map[String, List[String]] = {
      lines match {
        case head :: tail =>
          head.take(3) match {
            case "val" => {
              val valInterface =
                (head :: tail
                  .takeWhile(line =>
                    line.take(3) != "val" && line.take(4) != "type"
                  )).mkString

              val separatorIndex = valInterface.indexOf(":")
              val id = valInterface.take(separatorIndex).drop(4).trim
              val types =
                valInterface
                  .drop(separatorIndex + 1)
                  .trim
                  .split("->")
                  .map(_.trim)
                  .toList

              processLine(tail, typesMapAcc + (id -> types))
            }
            case _ => processLine(tail, typesMapAcc)
          }
        case Nil => typesMapAcc
      }
    }

    val typesMap = processLine(file.getLines.toList, Map())
    file.close
    typesMap
  }

  def writeMergedFile(
      typesMap: Map[String, List[String]],
      inputFileName: String,
      outputFileName: String
  ) = {
    val let = "^(let|let rec)"
    val start = s"($let| and)"

    def typeRegExp(typeOfParam: String) = s"\\s*:\\s*$typeOfParam\\s*"

    def idWithType(id: String, idType: String) =
      s"(\\s*\\(\\s*$id${typeRegExp(idType)}\\)|\\s*$id${typeRegExp(idType)})"

    val paramWithoutType = "\\s+\\w+"

    def paramWithType(paramType: String) =
      s"\\s*\\(\\s*\\w+${typeRegExp(paramType)}\\)"

    def param(paramType: String) =
      s"($paramWithoutType|${paramWithType(paramType)})"

    def paramsRegExp(paramsType: List[String]) = paramsType
      .map(paramType => param(paramType))
      .mkString

    def letWithoutParamsAndWithoutType(id: String) =
      s"$start\\s+$id\\s*=".r

    def letWithoutParams(id: String, idType: String) =
      s"$start(\\s+$id|${idWithType(id, idType)})\\s*="

    def letWithParams(id: String, idType: String, paramsType: List[String]) =
      s"$start\\s+$id${paramsRegExp(paramsType)}(${typeRegExp(idType)})?\\s*="

    def letDefRegExp(id: String, idType: String, paramsType: List[String]) =
      s"(${letWithoutParams(id, idType)}|${letWithParams(id, idType, paramsType)})".r

    def addTypeToValDefWithoutType(
        id: String,
        line: String,
        idType: String
    ): String = {
      val letId = s"$start\\s+$id".r
      val endIdIndex = letId.findFirstMatchIn(line).get.end
      val equalIndex =
        endIdIndex + line.substring(endIdIndex).indexOf('=')

      line.take(endIdIndex) + s" : $idType " + line.drop(equalIndex)
    }

    def addTypeToParams(
        toReplace: String,
        types: List[String],
        acc: String
    ): String = {
      types match {
        case t :: remainingTypes => {
          val paramWithTypeMatch =
            s"^${paramWithType(t)}".r.findFirstMatchIn(toReplace)
          if (paramWithTypeMatch.isDefined) {
            addTypeToParams(
              toReplace.drop(paramWithTypeMatch.get.end),
              remainingTypes,
              acc + toReplace.take(paramWithTypeMatch.get.end)
            )
          } else {
            val paramEndIndex =
              s"^$paramWithoutType".r.findFirstMatchIn(toReplace).get.end
            addTypeToParams(
              toReplace.drop(paramEndIndex),
              remainingTypes,
              acc + " (" + toReplace.take(paramEndIndex) + s" : $t)"
            )
          }
        }
        case Nil => acc
      }
    }

    def addTypeToFunDefWithParams(
        id: String,
        line: String,
        types: List[String]
    ): String = {
      val idRegExp = s"$start\\s+$id".r
      val paramsIndex = idRegExp.findFirstMatchIn(line).get.end
      val equalIndex =
        paramsIndex + line.substring(paramsIndex).indexOf('=')

      val returnType = types.last
      line.take(paramsIndex) + addTypeToParams(
        line.drop(paramsIndex),
        types.init,
        ""
      ) + s" : $returnType " + line.drop(equalIndex)
    }

    def addTypeToId(id: String, line: String, types: List[String]): String =
      types match {
        // case val def
        case head :: Nil => {
          val idWithTypeRegExp =
            s"$start${idWithType(id, head)}\\s*=".r

          idWithTypeRegExp.findFirstMatchIn(line) match {
            case Some(_) => line
            case None    => addTypeToValDefWithoutType(id, line, head)
          }
        }
        // case let def with multiple params or anonymous fun def
        case head :: tl => {
          letWithoutParamsAndWithoutType(id).findFirstMatchIn(line) match {
            // Anonymous fun def
            case Some(_) =>
              addTypeToValDefWithoutType(id, line, types.mkString(" -> "))
            // Function def with params
            case None => addTypeToFunDefWithParams(id, line, types)
          }
        }
        // Should not happen
        case Nil => line
      }

    def mergeTypesInLines(
        idWithTypes: List[(String, List[String])],
        currMergedLinesWithIndex: List[(String, Int)]
    ): List[(String, Int)] = idWithTypes match {
      case head :: tail => {
        val id = head._1

        val letDef = letDefRegExp(id, head._2.last, head._2.init)

        val idOccurrences = currMergedLinesWithIndex.filter {
          case (line, index) =>
            letDef.findFirstMatchIn(line).isDefined
        }

        val newMergedLines = idOccurrences match {
          case lineWithOccurrence :: Nil => {
            val newLine = addTypeToId(id, lineWithOccurrence._1, head._2)
            currMergedLinesWithIndex.map { case (line, index) =>
              if (index == lineWithOccurrence._2) (newLine, index)
              else (line, index)
            }
          }
          // TODO: manage the case where the id is found multiple times
          // TODO: print to the user that we could not merge the type and ask them to do so?
          case _ => currMergedLinesWithIndex
        }

        mergeTypesInLines(tail, newMergedLines)
      }
      case Nil => currMergedLinesWithIndex
    }

    val inputFile = fromFile(inputFileName)
    val inputLines = inputFile.getLines.toList

    val outputLines = mergeTypesInLines(
      typesMap.toList,
      inputLines.zipWithIndex
    ).sortBy { case (line, index) => index }.map { case (line, index) => line }

    val outputFile = new File(outputFileName)
    val writer = new PrintWriter(outputFile)
    writer.print(outputLines.mkString("\n"))

    inputFile.close
    writer.close
  }

}
