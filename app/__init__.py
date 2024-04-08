from flask import Flask, render_template, request
import compiler.basic as basic

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

# @app.route('/compile', methods=['POST'])
# def compile_code():
#     code = request.form['code']
#     code = code.replace("PRINT", "PRINT_RET")
#     result, error = basic.run('<stdin>', code)
#     if error:
#         return error.as_string()
#     elif result:
#         if len(result.elements) == 1:
#             return repr(result.elements[0])
#         else:
#             return repr(result)

# @app.route('/compile', methods=['POST'])
# def compile_code():
#     code = request.form['code']
#     # Replace instances of "PRINT" with "PRINT_RET"
#     code = code.replace("PRINT", "PRINT_RET")
    
#     print("Received code:", code)  # Debugging: Print the code received from the form
#     result, error = basic.run('<stdin>', code)
#     if error:
#         print("Compilation error:", error.as_string())  # Debugging: Print compilation errors
#         return error.as_string()
#     elif result:
#         if len(result.elements) == 0:
#             print("Result 1:", repr(result.elements[0]))  # Debugging: Print the result
#             return repr(result.elements[0])
#         else:
#             print("Result 2:", repr(result))  # Debugging: Print the result
#             return repr(result)

@app.route('/compile', methods=['POST'])
def compile_code():
    code = request.form['code']

    code = code.replace("PRINT", "PRINT_RET") # So that the output does not print in the console

    result, error = basic.run('<stdin>', code)

    if error:
        return error.as_string()
    elif result:
        output_elements = []
        for item in result.elements:
            if not str(item).startswith('<compiler'):
                output_elements.append(str(item))
        output = "\n".join(output_elements)
        return output
    
        # output = '\n'.join(map(str, result.elements))  # Join elements of the list with newline characters
       
        # output_elements = [str(item) for item in result.elements if not callable(item)]
        # output = '\n'.join(output_elements)  # Join elements of the list with newline characters
        # return output


if __name__ == '__main__':
    app.run(debug=True)
