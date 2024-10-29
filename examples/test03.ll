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
%4 = add i32 0, 1
%5 = add i32 0, 1
%6 = add i32 0, 1
%7 = add i32 0, 1
%8 = add i32 0, 1
%9 = add i32 0, 1
%10 = add i32 0, 1
%11 = add i32 0, 1
%12 = add i32 0, 1
%13 = add i32 0, 1
%14 = add i32 0, 1
%15 = add i32 0, 1
%16 = add i32 0, 1
%17 = add i32 0, 1
%18 = add i32 0, 1
%19 = add i32 0, 1
%20 = add i32 0, 1
%21 = add i32 0, 1
%22 = add i32 0, 1
%23 = add i32 0, 1
%24 = add i32 0, 1
%25 = add i32 0, 1
%26 = add i32 0, 1
%27 = add i32 0, 1
%28 = add i32 0, 1
%29 = add i32 0, 1
%30 = add i32 0, 1
%31 = add i32 0, 1
%32 = add i32 0, 1
%33 = add i32 0, 1
%34 = add i32 0, 1
%35 = add i32 0, 1
%36 = add i32 0, 1
%37 = add i32 0, 1
%38 = add i32 0, 1
%39 = add i32 0, 1
%40 = add i32 0, 1
%41 = add i32 0, 1
%42 = add i32 0, 1
%43 = add i32 %41, %42
%44 = add i32 %40, %43
%45 = add i32 %39, %44
%46 = add i32 %38, %45
%47 = add i32 %37, %46
%48 = add i32 %36, %47
%49 = add i32 %35, %48
%50 = add i32 %34, %49
%51 = add i32 %33, %50
%52 = add i32 %32, %51
%53 = add i32 %31, %52
%54 = add i32 %30, %53
%55 = add i32 %29, %54
%56 = add i32 %28, %55
%57 = add i32 %27, %56
%58 = add i32 %26, %57
%59 = add i32 %25, %58
%60 = add i32 %24, %59
%61 = add i32 %23, %60
%62 = add i32 %22, %61
%63 = add i32 %21, %62
%64 = add i32 %20, %63
%65 = add i32 %19, %64
%66 = add i32 %18, %65
%67 = add i32 %17, %66
%68 = add i32 %16, %67
%69 = add i32 %15, %68
%70 = add i32 %14, %69
%71 = add i32 %13, %70
%72 = add i32 %12, %71
%73 = add i32 %11, %72
%74 = add i32 %10, %73
%75 = add i32 %9, %74
%76 = add i32 %8, %75
%77 = add i32 %7, %76
%78 = add i32 %6, %77
%79 = add i32 %5, %78
%80 = add i32 %4, %79
%81 = add i32 %3, %80
%82 = add i32 %2, %81
%83 = add i32 %1, %82
call void @printInt(i32 %83)
  ret i32 0
}
