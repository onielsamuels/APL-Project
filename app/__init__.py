from flask import Flask, render_template, request

app = Flask(__name__)

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/submit', methods=['POST'])
def submit():
    field1 = request.form['field1']
    field2 = request.form['field2']
    return f'Field 1: {field1}, Field 2: {field2}'

if __name__ == '__main__':
    app.run(debug=True)
