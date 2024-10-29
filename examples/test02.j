.class public test02
.super java/lang/Object
.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit locals 1024
.limit stack 1024
getstatic java/lang/System/out Ljava/io/PrintStream;
bipush 44
iconst_2
isub
invokevirtual java/io/PrintStream/println(I)V
  return
.end method
