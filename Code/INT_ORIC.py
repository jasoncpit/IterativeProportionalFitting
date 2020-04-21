from random import choices
import math
import random
from itertools import chain
from decimal import Decimal,getcontext
from random import randint
from collections import defaultdict
import numpy as np 

class Int_method: 
    
    def __init__(self, input_list): 
        
        if isinstance(input_list, list):
            
            self.input_list = input_list
        else:
            
            raise TypeError 
   
    
    # Quick sort algorithm
    def quickSort(self, L, ascending=True):
        if len(L) <= 1:
            return L
        smaller, equal, larger = [], [], []
        pivot = L[randint(0, len(L) - 1)]

        for x in L:
            if x < pivot:
                smaller.append(x)
            elif x == pivot:
                equal.append(x)
            else:
                larger.append(x)

        larger = self.quickSort(larger, ascending=ascending)
        smaller = self.quickSort(smaller, ascending=ascending)

        if ascending:
            final = smaller + equal + larger
        else:
            final = larger + equal + smaller
        return final


    #quickSort(attr,ascending = False)

    # Find duplicated items and return their index
    @staticmethod
    def list_duplicates(seq):

        tally = defaultdict(list)

        for i, item in enumerate(seq):

            tally[item].append(i)

        return ((key, locs) for key, locs in tally.items()

                if len(locs) >= 1)

    def ORIC(self,tol = 1e-5,digit = 2,niteration = 99):
        
        #x should be a list 
        
        x = self.input_list
        list_duplicates =self.list_duplicates
        quickSort = self.quickSort
        m = [math.floor(i) for i in x]
        iteration = 0 
        store_pos,store_target = None,None 
        store_I = None 
        while True:
            
            # Set the precision.
            getcontext().prec = digit

            #Given datasets 

            #1. Set new list mi = floor(xi)

            #2. Compute constraint shortfall I(x) = sum(xi - mi) >= 0 

            I = []

            for index,value in enumerate(x):

                I.append(Decimal(value) - Decimal(m[index]))
            
            Isum = round(sum(I))

            #4. Sort indices in decreasing order of fractional part (Xi - Mi) 

            New_I  = quickSort(I,ascending = False) 

            #5. Sort each subsequence with each fractional parts (X_k - m_K) =...= (X_k+j - m_K+j) in decreasing order of integer parts mk >= mk+1 ...>= mk+j

                #Find subsequence with each fractional parts -> duplicated fractional part 

            dup_I = sorted(list_duplicates(I),reverse=True)


            rearranged_dup = []

            #sort fractional part in decreasing order of integer parts mk >= mk+1 ...>= mk+j

            for k in dup_I:

                #Dub_I: Sorted duplicate list based on I: subsequence with equal fractional parts 
                new_dup = k

                #Return Mk based on the sorted dub and its index 
                arranged_dup = [m[i] for i in new_dup[1]]

                #Return the index of the sorted list from return Mk 
                sorted_index = np.argsort(arranged_dup)


                #Find the index of the sorted_dup and sort dup_I

                new_dub_list = [new_dup[1][i] for i in sorted_index]

                new_dub_list2 = [new_dup[0],new_dub_list]    

                rearranged_dup.append(new_dub_list2)
                
            pos = []

            for k in rearranged_dup:

                new_dup = k[1]

                for j in range(len(new_dup)):

                    pos.append(new_dup[j])
                    
                    if len(pos) == Isum: 

                        break

                break
            #pos = pos[0:2]
            
            target = []
        
            for item in pos:

                #Set condition 

                error = x[item]-m[item] 

                if error > 0:

                    target.append(math.ceil(x[item]))

                elif error < 0: 

                    target.append(math.floor(x[item]))

            iteration += 1
            
            #print(iteration,Isum)

            #Comparing different status of M 
			
			

            if Isum > tol: 
                #If my I is bigger than tol, then update 
                print("Updating,convergence not found....")

                for item,value in zip(pos,target):

                    m[item] = value

                    store_pos,store_target,store_I = pos,target,Isum  

				
            if Isum == 0:
                #If I is found, then break 
                print("Convergence found", "Converge at iteration:",iteration,Isum)  
                            
                return(m)
                
                break
			

            if iteration > niteration: 
                
                print("Warning: Fail to converge! Please reset tolerance value.","Converge at iteration",iteration,Isum)
                
                return(m)
            
                break



			

if __name__ == "__main__":

    import os
    os.chdir("/Users/jasontang/desktop/IterativeProportionalFitting/Code/Method")
    import Integerisation
    import numpy as np 
    import pandas as pd
    #Relative error 
    def RE(X,Y):
        RE = []
        for i,j in zip(X,Y):
            error = abs(i - j)
            RE.append(error)
        return(RE)

    fw = pd.read_csv("/Users/jasontang/desktop/IterativeProportionalFitting/Code/Finalweigh.csv",index_col = False)
    fw_ipf = pd.DataFrame(fw.iloc[0:,1:]).T
    fw_ipf = fw_ipf.values.tolist()
    Input_list = fw_ipf[0] 

    solution = Int_method(Input_list).ORIC(tol=0.05, digit=10, niteration=1000)
    Output_STO = Integerisation.Integerisation(Input_list).int_sto()
    #fake_solution = [2, 0, 0, 2, 0, 0, 0, 1, 0]

    print(np.var(RE(solution,Input_list)),np.var(RE(Output_STO,Input_list)))
    print(sum(RE(solution,Input_list)),sum(RE(Output_STO,Input_list)))
    #print(solution,Output_STO)
