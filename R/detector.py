# Library import --------------------------------------------------------------------
print('\nLoading...')
import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '3'
import tensorflow as tf
from tensorflow.ragged import constant
from tensorflow.keras.applications import VGG16
from tensorflow.keras.layers import Flatten, Dense, Dropout, Input
from tensorflow.keras.optimizers import Adam
from tensorflow.keras import Sequential
from tensorflow.keras.preprocessing.image import load_img, img_to_array
import pandas as pd
pd.options.mode.chained_assignment = None  # default='warn'
tf.get_logger().setLevel('ERROR')
from PIL import Image
import requests
from io import BytesIO
# ------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
def ModelpredBi(url):
    response = requests.get(url)
    img_1 = Image.open(BytesIO(response.content))
    img_1 = img_to_array(img_1.resize(dim))
    img_1 = img_1.reshape((batch_size, img_1.shape[0], img_1.shape[1], img_1.shape[2]))
    img_1 = img_1/255
    inp = vgg(img_1)
    predict_class = topBI.predict(inp)
    shark_prob = round(predict_class[0][1], 2)
    print('detection probability: ' + str(shark_prob))
    return shark_prob
# image input ------------------------------------------------------------------------------
batch_size = 1
im_dim = 150
dim = (im_dim,im_dim)
img_width, img_height = im_dim, im_dim
# ------------------------------------------------------------------------------------------
# Pre-trained model -----------------------------------------------------------------------
vgg = VGG16(include_top=False, weights='imagenet',
            input_tensor=Input(shape=(img_width, img_height, 3)))         
# ------------------------------------------------------------------------------------------
# Shark Identifier -------------------------------------------------------------------------
topBI = Sequential()
topBI.add(Flatten(input_shape=vgg.layers[-1].output_shape[1:]))
topBI.add(Dense(units=400, activation='relu'))
topBI.add(Dropout(0.35))
topBI.add(Dense(2, activation='softmax'))
optBI = Adam(learning_rate=0.001)
topBI.compile(optimizer=optBI, loss='binary_crossentropy',
            metrics=['accuracy'])
topBI.load_weights('models/identifier_model.h5')

dat = pd.read_csv('dredge1.csv')
dat['detection'] = ''
for i in dat.index:
    url = dat['url_m'][i]
    dat['detection'][i]=ModelpredBi(url)