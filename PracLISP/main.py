import numpy as np

def canBeD(A, horizontal):
	if horizontal: return len(A)>1 #Horizontal division
	else: return len(A[0])>1 #Vertical division 

def divide(A, horizontal, first): #Divide&conquer function
	if horizontal:
		if first: 
			return A[:len(A)//2, ]
		else: 
			return A[len(A)//2:len(A), ]
	else:
		if first: 
			return A[:, :len(A[0])//2]
		else: 
			return A[:, len(A[0])//2:len(A[0])]

def setMatrix(a, b, c, d):
	return np.bmat([[a, b], [c, d]])

def multiply(A, B):
	if canBeD(A, True) and canBeD(B, False): #Recursive
		a, b, c, d=divide(A, True, True), divide(A, True, False), divide(B, False, True), divide(B, False, False)
		print("a")
		print(a)
		print("b")
		print(b)
		print("c")
		print(c)
		print("d")
		print(d)
		print("finnn")
		
		e, f, g, h=multiply(a, c), multiply(b, c), multiply(a, d), multiply(b, d)
		return setMatrix(e, g, f, h)
	else: #Base case
		return A.dot(B)

if __name__=='__main__':
	print("Ingrese los datos de la siguiente forma: filas columnas numero")
	# row1, col1, num1=map(int,input("Datos para la 1er matriz:  ").strip().split())
	# row2, col2, num2=map(int,input("Datos para la 2da matriz:  ").strip().split())
	# A=np.array([[num1 for i in range(col1)] for j in range(row1)])
	# B=np.array([[num2 for i in range(col2)] for j in range(row2)])
	A = np.array([[1, 2, 3, 4, 5], [4, 5, 1, 3, 5], [6, 7 ,8, 9, 10]])
	B = np.array([[1, 2], [3, 8], [9, 5], [7, 3], [10, 6]])
	print("\tMatriz A=\n\n{}\n\n\tMatriz B=\n\n{}\n\n\tProducto de AB=\n\n{}".format(A, B, multiply(A, B)))

