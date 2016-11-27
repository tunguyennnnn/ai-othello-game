# COMP472: Artificial Intelligence
# Tu Nguyen and Javier E. Fajardo

import sys, os, subprocess, shlex
import subprocess as sb
from threading import Timer

class Player():
	def __init__(self,color):
		self.color=color
	#end
#end class

class CPU_Player(Player):
	# Shorthand for Popen
	spawn = sb.Popen

	# Timeout before killing the process
	timeout = 5.0

	def __init__(self,color,calling_command,keep_alive=False,IN=sb.PIPE,OUT=sb.PIPE,ERR=sb.PIPE):
		self.color=color
		self.command=shlex.split(calling_command)
		self.process = None
		self.persistent = keep_alive

		self.STDIN=IN
		self.STDOUT=OUT
		self.STDERR=ERR
	#end init

	def play(self,serialized_board):
		"""Sends a serialized board to the process and returns a tuple with the move"""
		# Cleanup any old processes
		if not self.persistent and self.process is not None:
			while(self.process.poll() is None):
				self.process.kill()
		# end cleanup

		# open it up and start a timer
		self.process = self.spawn(self.command, stdin=self.STDIN,stdout=self.STDOUT,stderr=self.STDERR)
		watchdog = Timer(self.timeout,self.process.kill)
		watchdog.start()
		raw_reply = self.process.communicate(serialized_board)

		if watchdog.is_alive():
			watchdog.cancel()
		else:
			sys.stderr.write("Process timed out! That's a pass")
			return (-1,-1)

		response = raw_reply[0].strip().split()
		retVal=(-1,-1)
		try:
			retVal=tuple([int(x) for x in response])
			assert len(retVal)==2
		except Exception,e:
			print("CPU_Player Interaction ERROR")
			print(e)
			print("Process provided wrong output. Check command args or provision your program")
			print("CPU_Player: "+str(self.command))
			print("STDOUT: "+raw_reply[0])
			print("STDERR: "+raw_reply[1])
			exit(1) # kill all

		return retVal
	#end play

#end CPUPlayer
