# COMP472: Artificial Intelligence
# Javier E. Fajardo - 26487602
# datastructs.py: auxiliary data structures used in the assignment

class edge():
	def __init__(self):
		self.start_node = None
		self.end_node = None
		self.weight = 0
		self.label = None
	
	def connect(self,start,end):
		self.start_node=start
		self.end_node=end
		
	def __lt__(self, other):
		return self.weight < other.weight
	
	def __str__(self):
		return str(self.label)
		
	def __repr__(self):
		return "Edge <%s>" % (str(self.label))
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
		return "Node %s" % (str(self.value))
		
	def __lt__(self, other):
		return self.heuristic < other.heuristic

		
	def add_child(self,node,weight,label=None):
		new_edge = edge()
		new_edge.connect(self,node)
		new_edge.weight = weight
		new_edge.label = label
		self.edges.append(new_edge)
		
		node.root = self
		node.root_edge = new_edge
	#end add child
		
	def add_children(self,children):
		# we assume childs hasn't been added before
		num_children = len(children)
		new_nodes = [node(root=self,value=x,heuristic=0) for x in children]
		new_edges = [edge() for i in range(num_children)]
		for i in range(num_children):
			new_edges[i].connect(self,new_nodes[i])
			new_nodes[i].root_edge=new_edges[i]
	#end add_children
	
	def add_lazy_edges(self,new_edges,is_positive=False):
		if len(new_edges)<1:
			return
		# new edges is expected to be a list of tuples of the form (val,(x,y))
		modifier = 1
		if not is_positive:
			modifier = -1
		for info in new_edges:
			new_node = node(self,None,modifier*info[0])
			self.add_child(new_node,modifier*info[0],info[1])
		#end for
	#end add_lazy_edges
	
#end node
			
