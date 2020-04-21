import INT_ORIC 
import math
import numpy as np 

class TAE:
	def __init__(self,input_list,ORIC):

		self.input_list = input_list
		self.ORIC = ORIC

	@staticmethod
	def AE(input_list,ORIC):
		error_list = []

		for i,j in zip(input_list,ORIC): 
			error = abs(i - j)
			error_list.append(error/i)

		return(error_list)

	@staticmethod
	def TAE(input_list,ORIC):
		error_list = []

		for i,j in zip(input_list,ORIC): 
			error = abs(i - j)
			error_list.append(error)

		return(sum(error_list))

	
	def TAEmin(self):

		'''
		ORIC is the output from the ORIC algorithm 

		input_list is the original input 

		'''
		#GET INHERITENCE FUNCTION
		AE = self.AE 
		TAE = self.TAE 
		input_list = self.input_list
		ORIC = self.ORIC

		#1. Set xi = 1.. N, mi = optimised relative error set

		#Iteration 
		iteration = 0
		#Store position when no further TAE can be minimised -> and return the list from previous iteration  
		store_pos = 0

		#2. List that I want to update
		ORIC = ORIC

		while True:

			#3. Compute current constraints shortfall I(x) = xi-mi >= 0
			error_list =AE(input_list,ORIC)

			#4. Compute current total abosolute error  
			Old_TAE = TAE(input_list,ORIC)

			#5. Sort error list based on their index: 

			error_index = np.flip(np.argsort(error_list))

			store_pos = ORIC[error_index[0]]
			#6. Set Condition to minimise TAE	

			item = input_list[error_index[0]]-ORIC[error_index[0]]

			if item > 0:
		 		ORIC[error_index[0]] = ORIC[error_index[0]] + 1 
			elif item < 0:
		 		ORIC[error_index[0]] = ORIC[error_index[0]] - 1

		 	#7. Find difference of the current TAE  
			new_TAE = TAE(input_list,ORIC)

			TAE_diff = new_TAE - Old_TAE

			#8. If the TAE increases then return to the previous item 
			print(TAE_diff)
			if TAE_diff > 0:
				ORIC[error_index[0]] = store_pos

				return(ORIC)

				break 







def main(): 
	#Testing 
	import INT_ORIC
	#Relative error 
	def RE(X,Y):
		RE = []
		for i,j in zip(X,Y):
			error = abs(i - j)
			RE.append(error)
		return(RE)
	new_list = [1.88954301666355,
 0.0225309932322741,
 0.0385452246396423,
 1.88954301666355,
 0.0194772443299524,
 0.00258901653612837,
 0.0363060643658379,
 0.555378074595936,
 0.0385452246396423,
 0.0272603225210038,
 0.211493279698442,
 0.0482298390159595,
 0.0356180951751112,
 0.6999390907165369,
 1.56173063938384,
 1.88954301666355]
	print(new_list)
	Output = INT_ORIC.Int_method(new_list).ORIC(tol=0, digit=10, niteration=300)
	print(Output,sum(RE(Output,new_list)))
	updated_list = TAE(new_list,Output)
	print(updated_list.TAEmin(),sum(RE(updated_list.TAEmin(),new_list)))

if __name__ == '__main__':
	main()