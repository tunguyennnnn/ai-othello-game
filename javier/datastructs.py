# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# datastructs.py: auxiliary data structures used in the assignment

class edge():

	def __init__(self):
		self.start_node = None
		self.end_node = None
		self.weight = 0
	
	def __init__(self, start, end, weight):
		self.start_node = start
		self.end_node = end
		self.weight = weight
	
	def connect(self,start,end):
		self.start_node=start
		self.end_node=end

#end edge

class node():
	"""
	A tree type node. Can have n-children 
	"""	
	def __init__(self, root=None, value=None, heuristic=None):
		self.label = ""
		self.edges = []
		self.heuristic = heuristic
		self.value = value
		self.root = root
		
	def __str__(self):
		return str(self.value)
		
	def __repr__(self):
		return "Node <{}>".format(self.value)
		
	def __lt__(self, other):
		return self.heuristic < other.heuristic

		
	def add_child(self,node,weight):
		new_edge = edge()
		new_edge.connect(self,node)
		new_edge.weight = weight
		
		node.root = self
		node.root_edge = new_edge

#end node
			