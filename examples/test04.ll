@dnl = internal constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define void @printInt(i32 %x) {
       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
       call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
       ret void
}

define i32 @main(i32 %argc, i8** %argv) {
%1 = add i32 0, 1
%2 = add i32 0, 2
%3 = add i32 0, 1
%4 = add i32 0, 1
%5 = add i32 0, 1
%6 = add i32 0, 1
%7 = add i32 0, 1
%8 = add i32 0, 1
%9 = add i32 0, 1
%10 = add i32 %1, %2
%11 = add i32 %9, %10
%12 = add i32 %1, %11
%13 = add i32 %1, %12
%14 = add i32 %8, %13
%15 = add i32 %1, %14
%16 = add i32 %1, %15
%17 = add i32 %7, %16
%18 = add i32 %1, %17
%19 = add i32 %1, %18
%20 = add i32 %1, %19
%21 = add i32 %1, %20
%22 = add i32 %6, %21
%23 = add i32 %1, %22
%24 = add i32 %1, %23
%25 = add i32 %1, %24
%26 = add i32 %1, %25
%27 = add i32 %1, %26
%28 = add i32 %1, %27
%29 = add i32 %1, %28
%30 = add i32 %1, %29
%31 = add i32 %1, %30
%32 = add i32 %1, %31
%33 = add i32 %5, %32
%34 = add i32 %2, %33
%35 = add i32 %1, %34
%36 = add i32 %1, %35
%37 = add i32 %1, %36
%38 = add i32 %4, %37
%39 = add i32 %1, %38
%40 = add i32 %1, %39
%41 = add i32 %1, %40
%42 = add i32 %1, %41
%43 = add i32 %1, %42
%44 = add i32 %3, %43
%45 = add i32 %1, %44
%46 = add i32 %1, %45
%47 = add i32 %2, %46
call void @printInt(i32 %47)
  ret i32 0
}
