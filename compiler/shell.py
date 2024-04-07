import compiler.basic as basic

#Creating a loop that will read the input from the terminal window
while True:

	text = input('yabcc > ')
	if text.strip() == "": continue
	
	#Printing the output to the screen or terminal
	#We use stdin as a placeholder because the code is not from an actual file
	result, error = basic.run('<stdin>', text)

	#If there is an error we will print the error to the screen with our
	#error as string method
	if error: 
		print(error.as_string())
	#Else we will be able to print the result
	elif result: 
		if len(result.elements) == 1:
			print(repr(result.elements[0]))
		else:
			print(repr(result))