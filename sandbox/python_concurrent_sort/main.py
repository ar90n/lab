import random
from queue import Queue
from threading import Semaphore, Thread
import itertools

def main():
    n = 128
    data = [random.randint(0, 1024) for _ in  range(n)]
    qsort(data)
    print(data)


def qsort(data):
    q = Queue()
    q.put((0, len(data) - 1))

    task_count = Semaphore()
    finish_counter = itertools.count()
    next(finish_counter)
    finished = False

    class SortImpl(Thread):
        def __init__(self, n_threads):
            super().__init__()
            self._n_threads = n_threads

        def run(self):
            nonlocal finished
    
            while True:
                v = task_count.acquire()
                if finished:
                    break
                lh, rh = q.get()

                c = next(finish_counter)
                if len(data) == c:
                    finished = True
                    task_count.release(self._n_threads)
                elif lh < rh:
                    piv = partition(data, lh, rh)
                    if lh < piv:
                        q.put((lh, piv - 1))
                        task_count.release()
                    if piv < rh:
                        q.put((piv + 1, rh))
                        task_count.release()

    n_threads = 8
    threads = [SortImpl(n_threads) for _ in range(n_threads)]
    for th in threads:
        th.start()
    for th in threads:
        th.join()

def partition(data, lh, rh):
    pivot_index = lh
    pivot = data[lh]
    lh += 1;

    beg = lh
    end = rh
    while lh < rh:
        if data[rh] < data[lh]:
            data[lh], data[rh] = data[rh], data[lh]

        while lh <= end and data[lh] < pivot:
            lh += 1
        while beg <= rh and pivot < data[rh]:
            rh -= 1

    data[pivot_index], data[rh] = data[rh], data[pivot_index]
    return rh


if __name__ == "__main__":
    main()