from joblib import Memory, func_inspect
import typer

memory = Memory("/tmp")

@memory.cache
def func1():
    _, func_name = func_inspect.get_func_name(func1)
    print(f"{func_name} is called")

@memory.cache
def func2():
    _, func_name = func_inspect.get_func_name(func2)
    print(f"{func_name} is called")


def main():
    func1()
    func2()


if __name__ == "__main__":
    typer.run(main)
