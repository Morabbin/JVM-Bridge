module Main where
{
    import Data.Nothing;
    import Platform.JavaVM;
    import Foreign.JavaVM.VM;

    main :: IO ();
    main = vmRun [] (do
    {
        exClass <- vmFindClass "java/lang/Exception";
        exCons <- vmGetConstructor exClass [];
        exObj <- vmNewObject exClass exCons [];
        
        vmCatch (do
        {
            vmThrow exObj;
            putStrLn ("not thrown");
        })
        (\ex -> do
        {
            exn <- getIsNothing ex;
            putStrLn ("caught exception: "++(show (not exn)));
        })
    });
}

