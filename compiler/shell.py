# import compiler.yabcc as yabcc
import yabcc as yabcc

print("                                        ")
print("                                        ")
print("██    ██  █████  ██████   ██████  ██████")
print(" ██  ██  ██   ██ ██   ██ ██      ██     ")
print("  ████   ███████ ██████  ██      ██     ")
print("   ██    ██   ██ ██   ██ ██      ██     ")
print("   ██    ██   ██ ██████   ██████  ██████")
print("                                        ")
print("  Yet Another BASIC Compiler Compiler   ")
print("                                        ")
print("                                        ")

while True:
	text = input('yabcc > ')
	if text.strip() == "": continue
	result, error = yabcc.run('<stdin>', text)

	if error:
		print(error.as_string())
	elif result:
		if len(result.elements) == 1:
			print(repr(result.elements[0]))
		else:
			print(repr(result))