data Vehicle = Cycle Int
             | Car String
	     | FireEngine
	     | Transporter [Vehicle]
	 deriving (Eq)


numWheels :: Vehicle -> Int
numWheels (Cycle wheels) = wheels
numWheels (Car _) = 4
numWheels FireEngine = 6
numWheels (Transporter vs) = 12 + sum (map numWheels vs)

showWheels :: Vehicle -> String
showWheels v = let n = numWheels v
               in replicate n 'o'


instance Show Vehicle where
  show = showWheels

