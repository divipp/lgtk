{-# LANGUAGE FlexibleInstances #-}
module GUI.MLens.Gtk.IO
    ( runI
    ) where

import Control.Category
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Free
import Data.Maybe
import Prelude hiding ((.), id)

import Graphics.UI.Gtk

import Data.MLens.Ref
import Control.MLens.NewRef
import Control.MLens.NewRef.Unsafe ()
import GUI.MLens.Gtk.Interface

------------------

-- | (remove action, toggle action, show action)
type WriterState = (IO (), IO (), IO ())

type IOWriterState = WriterT WriterState IO

-- | Run an @IO@ parametrized interface description with Gtk backend
runI :: I IO -> IO ()
runI i = do
    _ <- initGUI
    dca <- newRef []
    rea <- newRef True
    (c, _) <- runWriterT $ userr_ rea dca i
    window <- windowNew
    set window [ containerBorderWidth := 10, containerChild := c ]
    _ <- window `on` deleteEvent $ liftIO (mainQuit) >> return False
    widgetShowAll window
    mainGUI
 where
    userr_ :: Ref IO Bool -> Ref IO [Ref IO (Maybe (Bool, IO ()))] -> I IO -> IOWriterState Widget
    userr_ rea dca i = case i of
        Button s m -> do
            w <- lift'' buttonNew
            lift $ evalFree (maybe (return ()) ((\x -> on w buttonActivated x >> return ()) . react))
                        ((\x -> on w buttonActivated x >> return ()) . react . join . fmap (maybe (return ()) id) . join . fmap (induce id)) m
            s >>=.. buttonSetLabel w
            fmap isJust m >>=.. widgetSetSensitive w
            return' w
        Entry k -> do
            w <- lift'' entryNew
            _ <- lift $ on w entryActivate $ react $ entryGetText w >>= writeRef k
            readRef k >>=. entrySetText w
            return' w
        Checkbox k -> do
            w <- lift'' checkButtonNew
            _ <- lift $ on w toggled $ react $ toggleButtonGetActive w >>= writeRef k
            readRef k >>=. toggleButtonSetActive w
            return' w
        Combobox ss k -> do
            w <- lift'' comboBoxNewText
            lift $ flip mapM_ ss $ comboBoxAppendText w
            _ <- lift $ on w changed $ react $ fmap (max 0) (comboBoxGetActive w) >>= writeRef k 
            readRef k >>=. comboBoxSetActive w
            return' w
        List o xs -> do
            w <- lift' $ case o of
                Vertical -> fmap castToBox $ vBoxNew False 1
                Horizontal -> fmap castToBox $ hBoxNew False 1
            flip mapM_ xs $ flattenI' >=> containerAdd'' w
            return' w
        Notebook xs -> do
            w <- lift' notebookNew
            flip mapM_ xs $ \(s, i) ->
                flattenI' i >>= lift . flip (notebookAppendPage w) s
            return' w
        Label s -> do
            w <- lift'' $ labelNew Nothing
            s >>=.. labelSetLabel w
            return' w
        Action m -> 
            lift m >>= flattenI'
        Cell False m f -> do
            w <- lift' $ alignmentNew 0 0 1 1
            cancelc <- lift $ newRef mempty
            togglec <- lift $ newRef mempty
            showc <- lift $ newRef mempty
            let cc = (readRef cancelc >>= id) >> writeRef cancelc mempty >> writeRef togglec mempty >> writeRef showc mempty
            let cc' = readRef togglec >>= id
            let cc'' = readRef showc >>= id
            tell (cc, cc', cc'')
            m >>=. \new -> do
                cc
                containerForeach w $ containerRemove w
                (x, (c1, c2, c3)) <- runWriterT $ flattenI' (f new)
                writeRef cancelc c1
                writeRef togglec c2
                writeRef showc c3
                containerAdd w x
                widgetShowAll w
            return' w
        Cell True m f -> do
            w <- lift' $ hBoxNew False 1
            tri <- lift $ newRef []
            cancelc <- lift $ newRef mempty
            togglec <- lift $ newRef mempty
            showc <- lift $ newRef mempty
            let cc = (readRef cancelc >>= id) >> writeRef cancelc mempty >> writeRef togglec mempty >> writeRef showc mempty
            let cc' = readRef togglec >>= id
            let cc'' = readRef showc >>= id
            tell (cc, cc', cc'')
            m >>=. \new -> do
                cc'
                containerForeach w $ widgetHideAll
                t <- readRef tri
                case [b | (a,b) <-t, a == new] of
                    [] -> do
                        (x, (c1, c2, c3)) <- runWriterT $ flattenI' $ f new
                        modRef cancelc (>> c1)
                        containerAdd w x
                        widgetShowAll x
                        modRef tri ((new, (c2, c3)) :)
                        writeRef togglec c2
                        writeRef showc c3
                    [(c2, c3)] -> do
                        c2
                        c3
                        writeRef togglec c2
                        writeRef showc c3
            return' w
      where
        flattenI' = userr_ rea dca

        infixl 1 >>=.., >>=.

        m >>=.. f = evalFree (lift . f) ((>>=. f) . join . fmap (induce id)) m

        (>>=.) :: (Eq a) => IO a -> (a -> IO ()) -> IOWriterState ()
        get >>=. install = lift get >>= \x -> do
            v <- lift $ newRef x
            b <- lift $ newRef $ Just $ (,) True $ do
                x <- readRef v
                x' <- get
                when (x /= x') $ do
                    writeRef v x'
                    install x'
                    return ()
            lift $ modRef dca (b :)
            tell (writeRef b Nothing, modRef b $ fmap $ mapFst not, mempty)
            lift $ install x

        react :: IO () -> IO ()
        react a = do
            b <- readRef rea
            when b $ do
            writeRef rea False
            a
            xs <- readRef dca
            writeRef dca ([] :: [Ref IO (Maybe (Bool, IO ()))])
            let ff (Just (b, m)) = when b m >> return True
                ff Nothing = return False
            xs' <- filterM ((>>= ff) . readRef) . reverse $ xs
            modRef dca (++ reverse xs') 
            writeRef rea True

    return' :: GObjectClass x => x -> IOWriterState Widget
    return' = return . castToWidget

    lift' m = do
        x <- lift m
        tell (mempty, mempty, widgetShow (castToWidget x))
        return x

    lift'' m = do
        x <- lift m
        tell (mempty, mempty, widgetShowAll (castToWidget x))
        return x

    containerAdd'' w x = do
        a <- lift' $ alignmentNew 0 0 0 0
        lift $ containerAdd a x
        lift $ containerAdd w a
        lift $ set w [ boxChildPacking a := PackNatural ]

mapFst f (a, b) = (f a, b)

instance Monoid (IO ()) where
    mempty = return ()
    mappend = (>>)


