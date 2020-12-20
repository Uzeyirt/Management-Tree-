
data Tree1 =Leaf String Float | Vertex String Float Tree1 Tree1 deriving Show




insertLeaf::String -> Float ->  Tree1
managementtree1::[String] -> [Float] -> [String] -> [Float] -> Tree1
insertVertex::[String] -> [Float] -> [String] -> [Float] -> Tree1 -- i added this one and insertLeaf fuctions because we need different implementations for both vertexs and leafs.
																  --Actually i made two versions for all of fuctions,one is for vertex, other is  for leaf.
supervisor::Tree1 -> String -> String
peers::Tree1 -> String -> String 
managers::Tree1 -> String -> [String]

avgsalary::Tree1 -> Float

managementtree1 s f s1 f1 =insertVertex s f ([last s1] ++  (init s1)) ([last f1] ++  (init f1)) -- in sample run, root is at the end of second list,that is why i did such an implementation. 




insertVertex s f (a:s1) (b:f1) =if ((length s1) > 1) 
								then (Vertex a b (insertVertex (take ((length s) `div` 2) s) (take ((length f) `div` 2) f) (take ((length s1) `div` 2) s1) (take ((length f1) `div` 2) f1) ) (insertVertex (drop ((length s) `div` 2) s) (drop ((length f) `div` 2) f) (drop ((length s1) `div` 2) s1) (drop ((length f1) `div` 2) f1) ))
								else (Vertex a b (insertLeaf (head s) (head f)) (insertLeaf (last s) (last f)) )

insertLeaf a b =(Leaf a b)		 			
						
							
supervisor (Vertex s t (Vertex s1 f1 t3 t4) (Vertex s2 f2 t5 t6)) searchstring =if ((s1 == searchstring) || (s2 == searchstring)) then s 
																			   else if (s == searchstring) then "this is our root!!!"
																			   else (supervisor (Vertex s1 f1 t3 t4) searchstring) ++ (supervisor (Vertex s2 f2 t5 t6) searchstring)


supervisor (Vertex s t (Leaf s1 f1 ) (Leaf s2 f2 )) searchstring  = if ((s1 == searchstring) || (s2 == searchstring)) then s
																	else ""


peers (Vertex s t (Vertex s1 f1 t3 t4) (Vertex s2 f2 t5 t6)) ss=if (s1==ss) then s2 
																else if (s2==ss) then s1 
																else (peers (Vertex s1 f1 t3 t4) ss) ++ (peers (Vertex s2 f2 t5 t6) ss)

peers (Vertex s t (Leaf s1 f1 ) (Leaf s2 f2 )) ss = if (s1==ss) then s2 else if (s2==ss) then s1 else ""

managers (Vertex s t (Vertex s1 f1  (Leaf s11 f11) (Leaf s12 f12))  (Vertex s2 f2  (Leaf s21 f21) (Leaf s22 f22)) ) ss =if ((ss==s11) || (ss==s12)) then [s,s1] -- i know that this function implementation is bad programming style but i couldn't find any way to do it other than this implementation
																													  else if ((ss==s21) || (ss==s22)) then [s,s2]
																													  else if ((ss==s1) || (ss==s2)) then [s]
																													  else ["no manager!!"]
																													  
avgsalary (Vertex s f (Vertex s1 f1 a b) (Vertex s2 f2 a2 b2 ))=( f + (avgsalary (Vertex s1 f1 a b)) + (avgsalary (Vertex s2 f2 a2 b2 )) ) / 7
avgsalary (Vertex s f (Leaf s1 f1) (Leaf s2 f2)) = f + (avgsalary  (Leaf s1 f1)) + (avgsalary (Leaf s2 f2))
avgsalary (Leaf s f) = f  																													  