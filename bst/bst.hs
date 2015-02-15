data BST a = Node a (BST a) (BST a) | Leaf

insert :: (Ord a) => a -> BST a -> BST a
insert new Leaf = Node new Leaf Leaf
insert new (Node val left right) 
    | new <= val = Node val (insert new left) right
    | otherwise = Node val left (insert new right)

exists :: (Ord a) => a -> BST a -> Bool
exists x Leaf = False
exists x (Node val left right)
    | x < val = exists x left
    | x > val = exists x right
    | otherwise = True

delete :: (Ord a) => a -> BST a -> BST a
delete x Leaf = Leaf
delete x (Node val Leaf right) = right
delete x (Node val left@(Node v l r) right)
    | x == val = Node v (delete v left) right
    | x > val = Node val left (delete x right)
    | x < val = Node val (delete x left) right

inorder :: (Ord a) => BST a -> [a]
inorder Leaf = []
inorder (Node v l r) = (inorder l) ++ [v] ++ (inorder r)

main = print $ inorder $ insert 4 $ delete 10 $ insert 3 $ insert 0 $ insert 10 $ insert 1 Leaf 
