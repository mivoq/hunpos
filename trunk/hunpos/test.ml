let a = Array.create 5 (Obj.magic 0) ;;
a.(1) <- "ablak";
print_string a.(1);