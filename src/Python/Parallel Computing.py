import time, multiprocessing

# work division between threads

# filename is a dummy file to 
def extract(filename, range):
    print()

if __name__ == '__main__':
    start = time.perf_counter()
    processes = []

    for i in range(1, 4, 1):
        process = multiprocessing.Process(target = fx, args = [i])
        process.start()
    
    processes.append(process)
    
    for p in processes:
        p.join()
        
    end = time.perf_counter()
    print(f'Finished in {end-start} second(s)')  