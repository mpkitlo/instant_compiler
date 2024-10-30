GHC        = ghc
SRC 	   = src
INSC_LLVM  = insc_llvm
INSC_JVM   = insc_jvm 
EXAMPLES   = examples

.PHONY : all clean clean_examples

all : JVM LLVM

JVM :
	cd ${SRC} && ${GHC} ${GHC_OPTS} JVM -o ../${INSC_JVM} && cd ..

LLVM :
	cd ${SRC} && ${GHC} ${GHC_OPTS} LLVM -o ../${INSC_LLVM} && cd ..

clean :
	cd ${SRC} && rm -rf *.hi *.o && cd ..

clean_examples :
	cd ${EXAMPLES} && rm -rf *.j *.class *.ll *.bc && cd ..




