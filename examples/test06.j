.class public test06
.super java/lang/Object
.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1024
.limit stack 1024
iconst_0
istore_0
iconst_1
istore_1
iconst_0
istore_2
iconst_1
istore_3
iconst_0
istore 4
iconst_1
istore 5
iconst_0
istore 6
iconst_1
istore 7
getstatic java/lang/System/out Ljava/io/PrintStream;
iload_0
iload_1
imul
iload_2
iload_3
imul
iload 4
iload 5
iload 6
iload 7
iadd
iadd
iadd
iadd
iadd
invokevirtual java/io/PrintStream/println(I)V
iconst_1
istore_0
iconst_2
istore_1
iconst_1
istore_2
iconst_2
istore_3
iconst_1
istore 4
iconst_2
istore 5
iconst_1
istore 6
iconst_2
istore 7
iconst_1
istore 8
iconst_2
istore 9
iconst_1
istore 10
iconst_2
istore 11
iconst_1
istore 12
iconst_2
istore 13
getstatic java/lang/System/out Ljava/io/PrintStream;
iconst_2
iload_0
imul
iload_1
iconst_2
idiv
iload_2
iload_3
iload 4
iload 5
iload 6
iload 7
iload 8
iload 9
iconst_2
idiv
iload 10
iload 11
iload 12
iload 13
iadd
iadd
iadd
iadd
iadd
iadd
iadd
iadd
iadd
iadd
iadd
iadd
iadd
bipush 10
idiv
invokevirtual java/io/PrintStream/println(I)V
  return
.end method
