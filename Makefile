FC = gfortran
#WFC = x86_64-w64-mingw32-gfortran
FFLAGS = -Wall -Wextra -O2
LDFLAGS = -static

SRC = src
BUILD = build
TARGET = $(BUILD)/kinoshitran

.PHONY: all build run clean dir

all: build

build: $(TARGET) 

dir:
	mkdir -p $(BUILD)

# Production executables
$(TARGET): $(BUILD)/main.o $(BUILD)/kinoshita_class.o $(BUILD)/rng.o | dir 
	$(FC) $(LDFLAGS) $^ -o $@

# Production objects
$(BUILD)/rng.o: $(SRC)/rng.f90 | dir
	$(FC) $(FFLAGS) -J $(BUILD) -c $< -o $@

$(BUILD)/kinoshita_class.o: $(SRC)/kinoshita_class.f90 $(BUILD)/rng.o | dir
	$(FC) $(FFLAGS) -J $(BUILD) -I $(BUILD) -c $< -o $@

$(BUILD)/main.o: $(SRC)/main.f90 $(BUILD)/kinoshita_class.o $(BUILD)/rng.o | dir
	$(FC) $(FFLAGS) -I $(BUILD) -c $< -o $@

run: build
	./$(TARGET)

clean:
	rm -rf $(BUILD)