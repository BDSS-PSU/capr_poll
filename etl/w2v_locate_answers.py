# Load documents and locate them in the Google-Corpus-Word2Vec-Space
from __future__ import unicode_literals
from gensim.models import Word2Vec
import numpy as np
import io

def locate_document(doc, model):
    '''
    Calculate average location of document in vector space

    Args:
    ---------
    doc: list, a document as a list of words
    model: the word 2 vec model

    Returns:
    ---------
    A numpy array of dimension 1 x N, where N is the number of dimensions
       of the vector space of the model

    '''
    # Look up location of each word
    vectors = []
    for word in doc:
        try:
            vectors.append(model[word])
        except KeyError:
            pass
        
    # Average the vectors
    if len(vectors) == 0:
        location = [np.nan] * 300
    else:
        location = map(np.mean, zip(*vectors))    

    return location


def locate_file(in_path, out_path, model_path):
    '''
    Locate all docs in textfile in word 2 vec vector space

    Arguments:
    ---------
    in_path: Path to input file (one doc per line)
    out_path: Path to file for output
    model_path: Word2vec binary file

    '''

    infile = io.open(in_path, 'r', encoding='utf-8')
    outfile = io.open(out_path, 'w+', encoding='utf-8')
    print "Loading w2v vectors..."
    model = Word2Vec.load_word2vec_format(model_path, binary=True)

    # Generate the header for the outfile
    header = ["question_id", "case_id"]
    i = 0
    while i < 300:
        colname = "dim_%d" %i
        header.append(colname)
        i += 1
    header = ','.join(header) + '\n' 
    outfile.write(header)

    print "Locating documents"
    for i,line in enumerate(infile):
        
        # Skip header
        if i == 0:
            continue
        
        # Extract input components
        components = line.split(',')
        q_id = components[0]
        id_ = components[1]
        text = components[2].split(' ')
        
        # Locate the text in vector space
        location = locate_document(doc=text, model=model)

        # Write output to file
        str_location = ','.join([str(coord) for coord in location]) 
        out_line = '{},{},{}\n'.format(q_id, id_, str_location)
        outfile.write(out_line)

        if i % 100 == 0:
            print 'Processed {}'.format(i)

 
    infile.close()
    outfile.close()

    
if __name__ == '__main__':

    locate_file(in_path='../data/preproc_answers_no_manual_check/1_grams_text_answers_no_stopwords.txt',
            out_path='../data/text_answers_in_word2vec_space.csv',
            model_path='../data/word2vec_vectors.bin')
    
