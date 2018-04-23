# Trials

The results are stored in the following directories as `csv` files:

* `result/original/`: unpertubated
    * `result/original/nationwide.csv`
    * `result/original/countywide.csv`
    * `result/original/distrctwide.csv`
* `result/pertubated/`: pertubated with noises of different scales (15 each)
    * `result/pertubated/nationwide/`: Ɛ ranging from `2^-9` to `2^5`
    * `result/pertubated/countywide/`: Ɛ ranging from `2^-4` to `2^10`
    * `result/pertubated/distrctwide/`: Ɛ ranging from `2^6` to `2^20`

```
scale = 400000.0 / (population x Ɛ)
```
