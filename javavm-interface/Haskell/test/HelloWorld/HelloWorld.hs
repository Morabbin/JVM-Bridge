module Main where
{
    import Control.Monad.Reference;
    import Data.Witness;
    import Platform.JavaVM;
    import Foreign.JavaVM.VM;

    message :: JavaString;
    message = "Hello, World!";

    main :: IO ();
    main = vmRun [] (do
    {
        systemClass <- vmFindClass "java/lang/System";
        outRef <- vmGetStaticField systemClass (MkMemberNameType "out" (MkObjectType "java/io/PrintStream"));
        out <- get outRef;
        cref <- vmFindClass "java/io/PrintStream";
        method <- vmGetMethod cref (MkMemberNameType "println" (MkFunctionType [MkArrayType (MkPrimitiveType (MkAnyWitness Tchar))] MkVoidType));
        arr <- vmNewArray Tchar (fromIntegral (length message));
        vmSetArrayRegion arr 0 message;
        method Nothing out [mkAny arr];
    });
}

