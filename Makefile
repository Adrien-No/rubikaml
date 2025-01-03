##
# Project Title
#
# @file
# @version 0.1

draw:
	dune build --profile release; dune test --profile release
	dot -Kneato -Tpng graph.dot > graph.png
	eog graph.png &

# end
