let for1 = for i = 1 to 10 do
  print_endline "For loop to 10";
done;;
let for2 = for i = 10 downto 1 do
  print_endline "For loop downto 1";
done;;
let for3 = for i = 2 + 3 to 4 * 5 do
  print_endline "For loop with weird boundaries";
  print_endline "Wait a moment...";
done;;