import json
import re

def json_to_txt(json_path, orig_txt_path, samp_txt_path):
    with open(json_path, 'r') as f:
        data = json.load(f)
    with open(orig_txt_path, 'w') as f:
        for line in data["original"]:
            f.write(line + '\n')
    with open(samp_txt_path, 'w') as f:
        for line in data["sampled"]:
            line = line.strip()
            # replace "\n"s with " "
            line = re.sub(r'[\n]+', ' ', line)
            f.write(line + '\n')

# json_to_txt('./gpt-4/writing_gpt-4.raw_data.json', 
#             './gpt-4/writing_gpt-4.original.txt',
#             './gpt-4/writing_gpt-4.sampled.txt')
# json_to_txt('./gpt-3.5/writing_gpt-3.5-turbo.raw_data.json', 
#             './gpt-3.5/writing_gpt-3.5-turbo.original.txt',
#             './gpt-3.5/writing_gpt-3.5-turbo.sampled.txt')
# json_to_txt('./davinci/writing_davinci.raw_data.json', 
#             './davinci/writing_davinci.original.txt',
#             './davinci/writing_davinci.sampled.txt')

# json_to_txt('./gpt-4/pubmed_gpt-4.raw_data.json',
#             './gpt-4/pubmed_gpt-4.original.txt',
#             './gpt-4/pubmed_gpt-4.sampled.txt')
# json_to_txt('./gpt-3.5/pubmed_gpt-3.5-turbo.raw_data.json',
#             './gpt-3.5/pubmed_gpt-3.5-turbo.original.txt',
#             './gpt-3.5/pubmed_gpt-3.5-turbo.sampled.txt')
# json_to_txt('./davinci/pubmed_davinci.raw_data.json',
#             './davinci/pubmed_davinci.original.txt',
#             './davinci/pubmed_davinci.sampled.txt')

# json_to_txt('./gpt-4/xsum_gpt-4.raw_data.json',
#             './gpt-4/xsum_gpt-4.original.txt',
#             './gpt-4/xsum_gpt-4.sampled.txt')
# json_to_txt('./gpt-3.5/xsum_gpt-3.5-turbo.raw_data.json',
#             './gpt-3.5/xsum_gpt-3.5-turbo.original.txt',
#             './gpt-3.5/xsum_gpt-3.5-turbo.sampled.txt')
# json_to_txt('./davinci/xsum_davinci.raw_data.json',
#             './davinci/xsum_davinci.original.txt',
#             './davinci/xsum_davinci.sampled.txt')