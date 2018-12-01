#!/usr/bin/env python3
from keras.models import Sequential
from keras.layers import Dense


def main():
    model = Sequential()
    model.add(Dense(units=64, activation='relu', input_dim=100))
    model.add(Dense(units=10, activation='softmax'))
    model.compile(loss='categorical_crossentropy',
                  optimizer='sgd',
                  metrics=['accuracy'])
    print(model)


if __name__ == "__main__":
    main()
