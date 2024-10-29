@dnl = internal constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define void @printInt(i32 %x) {
       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
       call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
       ret void
}

define i32 @main(i32 %argc, i8** %argv) {
%1 = add i32 0, 44
%2 = add i32 0, 2
%3 = sub i32 %1, %2
call void @printInt(i32 %3)
  ret i32 0
}
