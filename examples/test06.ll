@dnl = internal constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define void @printInt(i32 %x) {
       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
       call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
       ret void
}

define i32 @main(i32 %argc, i8** %argv) {
%1 = add i32 0, 0
%2 = add i32 0, 1
%3 = add i32 0, 0
%4 = add i32 0, 1
%5 = add i32 0, 0
%6 = add i32 0, 1
%7 = add i32 0, 0
%8 = add i32 0, 1
%9 = mul i32 %1, %2
%10 = mul i32 %3, %4
%11 = add i32 %7, %8
%12 = add i32 %6, %11
%13 = add i32 %5, %12
%14 = add i32 %10, %13
%15 = add i32 %9, %14
call void @printInt(i32 %15)
%16 = add i32 0, 1
%17 = add i32 0, 2
%18 = add i32 0, 1
%19 = add i32 0, 2
%20 = add i32 0, 1
%21 = add i32 0, 2
%22 = add i32 0, 1
%23 = add i32 0, 2
%24 = add i32 0, 1
%25 = add i32 0, 2
%26 = add i32 0, 1
%27 = add i32 0, 2
%28 = add i32 0, 1
%29 = add i32 0, 2
%30 = add i32 0, 2
%31 = mul i32 %30, %16
%32 = add i32 0, 2
%33 = sdiv i32 %17, %32
%34 = add i32 0, 2
%35 = sdiv i32 %25, %34
%36 = add i32 %28, %29
%37 = add i32 %27, %36
%38 = add i32 %26, %37
%39 = add i32 %35, %38
%40 = add i32 %24, %39
%41 = add i32 %23, %40
%42 = add i32 %22, %41
%43 = add i32 %21, %42
%44 = add i32 %20, %43
%45 = add i32 %19, %44
%46 = add i32 %18, %45
%47 = add i32 %33, %46
%48 = add i32 %31, %47
%49 = add i32 0, 10
%50 = sdiv i32 %48, %49
call void @printInt(i32 %50)
  ret i32 0
}
