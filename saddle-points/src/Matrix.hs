module Matrix (saddlePoints) where

import Data.Array (Array, Ix, assocs)

saddlePoints :: (Ix a, Ix b, Eq e, Ord e) => Array (a,b) e -> [(a,b)]
saddlePoints = map fst . saddlePoints' . assocs
    where
        saddlePoints' xs = filter isSaddle xs
            where
                isSaddle ((a,b),e) = isMaxInRow && isMinInCol
                    where
                        row = filter ((== a) . fst . fst) xs
                        col = filter ((== b) . snd . fst) xs
                        isMaxInRow = not . any ((> e) . snd) $ row
                        isMinInCol = not . any ((< e) . snd) $ col