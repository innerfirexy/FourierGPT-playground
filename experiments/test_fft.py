# Test FFT
from scipy.fft import fft, fftfreq, fftshift

nll_str = '9.4209 8.7340 8.8040 8.5586 7.1054 9.3126 5.2741 7.2314 8.4236 14.0459 11.2261 2.8106 6.3148 6.4838 14.1911 13.8982 8.9006 6.6990 5.5197 7.0505 8.8761 3.3112 2.8385 14.9621 4.6101 12.5873 2.8336 8.9940 2.8293 9.5401 9.5607 10.5070 3.1954 2.8800 5.0455 8.8794 8.4269 7.2003' # First line of "data/gpt-4/pubmed_Ans_gpt-4.original.gpt2.nll.txt"
data = np.array([float(x) for x in nll_str.split()])
N = data.shape[-1]
freq_x = fftshift(fftfreq(N))
fft_res = fftshift(fft(data))

sp_x = fft_res.real

print(freq_x[len(freq_x)//2:])
print(sp_x[len(sp_x)//2:])
sp_x = fft_res.imag
print(sp_x[len(sp_x)//2:])
sp_x = np.abs(fft_res)
print(sp_x[len(sp_x)//2:])
sp_x = np.sqrt(fft_res.real**2 + fft_res.imag**2)
print(sp_x[len(sp_x)//2:])