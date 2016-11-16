import sys

def main(args):
	print(args)
	#sys.stdout.flush()
	some_input = raw_input()
	print(some_input)
	some_input = raw_input()
	print(some_input)
#end main
	
if __name__=="__main__":
	main(sys.argv[1:])