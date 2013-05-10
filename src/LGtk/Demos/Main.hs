{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
module LGtk.Demos.Main
    ( main
    ) where

import Control.Monad
import Control.Concurrent
import System.Environment
import Data.Maybe
import Prelude hiding (id, (.))

import LGtk

import LGtk.Demos.Tri
import LGtk.Demos.IntListEditor
import LGtk.Demos.TEditor

main :: IO ()
main = runWidget $ notebook
    [ (,) "Hello" $ Label $ constSend "Hello World!"

    , (,) "Counters" $ notebook

        [ (,) "Unbounded" $ Action $ do
            c <- newRef 0
            return $ vcat
                [ Label $ rEffect $ liftM show $ readRef c
                , hcat
                    [ smartButton (constSend "+1") (return . (+1)) c
                    , smartButton (constSend "-1") (return . (+(-1))) c
                    ]
                ]

        , (,) "1..3" $ Action $ do
            c <- newRef 1
            return $ vcat
                [ Label $ rEffect $ liftM show $ readRef c
                , hcat
                    [ smartButton (constSend "+1") (return . min 3 . (+1)) c
                    , smartButton (constSend "-1") (return . max 1 . (+(-1))) c
                    ]
                ]

        , (,) "a..b" $ Action $ do
            a <- newRef 1
            b <- newRef 3
            let a' = joinRef $ do
                    bv <- readRef b
                    return $ lens id (const . min bv) % a
            let b' = joinRef $ do
                    av <- readRef a
                    return $ lens id (const . max av) % b
            return $ vcat
                [ counter a' b'
                , hcat [ Label $ constSend "min", entry $ showLens % a' ]
                , hcat [ Label $ constSend "max", entry $ showLens % b' ]
                ]

        ]

    , (,) "TabSwitch" $ Action $ do
        x <- newRef "a"
        let w = vcat [ Label $ rEffect $ readRef x, entry x ]
        return $ notebook
            [ (,) "T1" w
            , (,) "T2" w
            ]

    , (,) "Async" $ Action $ do
        ready <- newRef True
        delay <- newRef 1.0
        let f = do
                b <- readRef ready
                if b then return Nothing else liftM Just $ readRef delay
            g (Just d) re = liftEffectM $ void $ forkIO $ threadDelay (ceiling $ 10^6 * d) >> re True
            g _ _ = return ()
        async (toReceive $ writeRef ready) $ asyncToSend False f g
        return $ vcat
            [ hcat [ entry $ showLens % delay, Label $ constSend "sec" ]
            , Button (rEffect $ readRef delay >>= \d -> return $ "Start " ++ show d ++ " sec computation") (rEffect $ readRef ready) $ toReceive $ const $ writeRef ready False
            , Label $ rEffect $ liftM (\b -> if b then "Ready." else "Computing...") $ readRef ready
            ]

    , (,) "Timer" $ Action $ do
        t <- newRef 0
        let g t re = liftEffectM $ void $ forkIO $ threadDelay (ceiling $ 10^6 * 1) >> re (1+t)
        async (toReceive $ writeRef t) $ asyncToSend False (readRef t) g
        return $ vcat
            [ Label $ rEffect $ readRef $ showLens % t
            ]

    , (,) "System" $ notebook

        [ (,) "Args" $ Action $ liftEffectM getArgs >>= \args -> return $ Label $ constSend $ unlines args

        , (,) "ProgName" $ Action $ liftEffectM getProgName >>= \args -> return $ Label $ constSend args

        , (,) "Env" $ Action $ do
            v <- newRef "HOME"
            return $ vcat
                [ entry v
                , Label $ \re -> rEffect (readRef v) $ \s -> lookupEnv s >>= re . maybe "Not in env." show
                ]

        , (,) "Std I/O" $ let
            put = hcat [ Label $ constSend "putStrLn", Entry (voidSend, \re -> liftEffectM $ re $ \x -> putStrLn x) ]
            get = Action $ do
                ready <- newRef $ Just ""
                let g False re = liftEffectM $ void $ forkIO $ getLine >>= re
                    g _ _ = return ()
                async (toReceive $ writeRef ready . Just) $ asyncToSend False (liftM isJust $ readRef ready) g
                return $ hcat 
                    [ Button (constSend "getLine") (rEffect $ liftM isJust $ readRef ready) $ toReceive $ const $ writeRef ready Nothing
                    , Label $ rEffect $ liftM (maybe "" id) $ readRef ready
                    ]
           in vcat [ put, put, put, get, get, get ]
        ]

    , (,) "IntListEditor" $ Action $ do
        state <- fileRef "intListEditorState.txt"
        settings <- fileRef "intListEditorSettings.txt"
        return $ intListEditor (justLens "" % state) (justLens "" % settings)
    , (,) "Tri" tri
    , (,) "T-Editor1" tEditor1
    , (,) "T-Editor3" $ Action $ newRef (iterate (Node Leaf) Leaf !! 10) >>= tEditor3
    ]

justLens :: a -> Lens (Maybe a) a
justLens a = lens (maybe a id) (const . Just)

counter a b = Action $ do
    c <- newRef 0
    let c' = joinRef $ do
            av <- readRef a
            bv <- readRef b
            return $ iso (min bv . max av) id % c
    return $ vcat
        [ Label $ rEffect $ liftM show $ readRef c'
        , hcat
            [ smartButton (constSend "+1") ((\x -> liftM (min x) $ readRef b) . (+1)) c'
            , smartButton (constSend "-1") ((\x -> liftM (max x) $ readRef a) . (+(-1))) c'
            ]
        ]

