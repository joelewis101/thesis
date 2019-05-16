BLASTs were done by 


wget https://raw.githubusercontent.com/thanhleviet/ISfinder-sequences/master/IS.fna ISfinder.db

cat ctxm1/ctxm1.cdhit ctxm14/ctxm14.cdhit ctxm15/ctxm15.cdhit ctxm16/ctxm16.cdhit ctxm27/ctxm27.cdhit ctxm3/ctxm3.cdhit ctxm9/ctxm9.cdhit shv12/shv12.cdhit  > allAMRcontigs.fa 
 
blastn -db IS.fna.db -query ../allAMRcontigs.fa -outfmt 10 > allAMRcontigs.blast.IS.csv


## SRST2

makeblastdb -in ARGannot_r3.fasta -dbtype nucl -title SRST2.db -out SRST2.db


blastn -db SRST2.db -query ../allAMRcontigs.fa -outfmt 10 > allAMRcontigs.blast.SRST2.csv

## Plasmidfinder


wget https://bitbucket.org/genomicepidemiology/plasmidfinder_db/raw/master/enterobacteriaceae.fsa


makeblastdb -in enterobacteriaceae.fsa -dbtype nucl -title pfinder.db -out pfinder.db

blastn -db pfinder.db -query ../allAMRcontigs.fa -outfmt 10 > allAMRcontigs.blast.pfinder.csv
 
