
def make_counter():
    i = 0
    def count():
        i = 0
        i += 1
        print(i)

    return count

c = make_counter()
c()
c()
