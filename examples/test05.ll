@dnl = internal constant [4 x i8] c"%d\0A\00"

declare i32 @printf(i8*, ...)

define void @printInt(i32 %x) {
       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0
       call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
       ret void
}

define i32 @main(i32 %argc, i8** %argv) {
%1 = add i32 0, 1
%2 = add i32 0, 1
%3 = add i32 0, 1
%4 = sub i32 %2, %3
%5 = add i32 0, 1
%6 = add i32 0, 1
%7 = sub i32 %5, %6
%8 = add i32 0, 1
%9 = add i32 0, 1
%10 = sub i32 %8, %9
%11 = add i32 0, 1
%12 = add i32 0, 1
%13 = sub i32 %11, %12
%14 = add i32 0, 1
%15 = add i32 0, 1
%16 = sub i32 %14, %15
%17 = add i32 0, 1
%18 = add i32 0, 1
%19 = sub i32 %17, %18
%20 = add i32 0, 1
%21 = add i32 0, 1
%22 = sub i32 %20, %21
%23 = add i32 0, 1
%24 = add i32 0, 1
%25 = sub i32 %23, %24
%26 = add i32 0, 1
%27 = add i32 0, 1
%28 = sub i32 %26, %27
%29 = add i32 0, 1
%30 = add i32 0, 1
%31 = sub i32 %29, %30
%32 = add i32 0, 1
%33 = add i32 0, 1
%34 = sub i32 %32, %33
%35 = add i32 0, 1
%36 = add i32 0, 1
%37 = sub i32 %35, %36
%38 = add i32 0, 1
%39 = add i32 0, 1
%40 = sub i32 %38, %39
%41 = add i32 0, 1
%42 = add i32 0, 1
%43 = sub i32 %41, %42
%44 = add i32 0, 1
%45 = add i32 0, 1
%46 = sub i32 %44, %45
%47 = add i32 0, 1
%48 = add i32 0, 1
%49 = sub i32 %47, %48
%50 = add i32 0, 1
%51 = add i32 0, 1
%52 = sub i32 %50, %51
%53 = add i32 0, 1
%54 = add i32 0, 1
%55 = sub i32 %53, %54
%56 = add i32 0, 1
%57 = add i32 0, 1
%58 = sub i32 %56, %57
%59 = add i32 %55, %58
%60 = add i32 %52, %59
%61 = add i32 %49, %60
%62 = add i32 %46, %61
%63 = add i32 %43, %62
%64 = add i32 %40, %63
%65 = add i32 %37, %64
%66 = add i32 %34, %65
%67 = add i32 %31, %66
%68 = add i32 %28, %67
%69 = add i32 %25, %68
%70 = add i32 %22, %69
%71 = add i32 %19, %70
%72 = add i32 %16, %71
%73 = add i32 %13, %72
%74 = add i32 %10, %73
%75 = add i32 %7, %74
%76 = add i32 %4, %75
%77 = add i32 %1, %76
call void @printInt(i32 %77)
  ret i32 0
}
