from random import choices
import math
import random
from itertools import chain
from decimal import Decimal,getcontext
from random import randint
from collections import defaultdict
import numpy as np 

class Integerisation: 
    
    def __init__(self, input_list): 
        
        if isinstance(input_list, list):
            
            self.input_list = input_list
        else:
            
            raise TypeError 
   
    #Simple rounding 
    def int_sim(self):
        
        x = [round (i) for i in self.input_list]
        
        return(x)
    
    #Straight integerisation method 
    def int_int(self):
        
        x = [int (i) for i in self.input_list]
        
        return(x)

    #Stochastic rounding 
    def int_sto(self):
        
        rounding = []
        
        for x in self.input_list: 
            
            up,down = math.ceil(x),math.floor(x)

            choice_list = [down,up] 

            prob_list = [up-x,x-down]

            rounding.append(choices(choice_list, prob_list))
        

        return(sum(rounding,[]))

