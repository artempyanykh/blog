import sys

def main():
    if len(sys.argv) < 2:
        print("Usage: py-fizzbuzz <number>")
        sys.exit(0)

    try:
        n = int(sys.argv[1])
    except ValueError:
        print("Error: Not a number")
        sys.exit(0)

    solveFizzBuzz(n)

def solveFizzBuzz(n):
    for i in range(1, n + 1):
        rem3 = i % 3
        rem5 = i % 5

        if (rem3 == 0):
            print("Fizz", end="")

        if (rem5 == 0):
            print("Buzz", end="")
        else:
            if (rem3 != 0):
                print(i, end="")

        print("")

if __name__ == "__main__":
    main()
