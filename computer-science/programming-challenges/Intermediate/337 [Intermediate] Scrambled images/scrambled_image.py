from PIL import Image
import sys

def unscramble_image(image):
	width, height = image.size
	result = Image.new("RGBA", (width, height))

	for row in range(height):
		for col in range(width):
			pixel = image.getpixel((col, row))
			if not greyscale(pixel):
				circular_shift_pixels(image, result, row, col)
				break
		
	return result

def explore_image():
	image = Image.open(sys.argv[1])
	width, height = image.size

	print("\nGetting non-greyscale pixels along rows...\n")
	for row in range(height):
		for col in range(width):
			pixel = image.getpixel((col, row))
			if not greyscale(pixel):
				print(col, end=" ")
		print()

	print("\nGetting non-greyscale pixels along columns...\n")
	for col in range(width):
		for row in range(height):
			pixel = image.getpixel((col, row))
			if not greyscale(pixel):
				print(row, end=" ")
		print()


def circular_shift_pixels(image, result, row, col_index):
	width, height = image.size

	for col in range(width):
		new_col = (col + col_index) % width
		new_pixel = image.getpixel((new_col, row))
		result.putpixel((col, row), new_pixel)


def greyscale(pixel):
	return pixel[0] == pixel[1] == pixel[2]

def main():
	image = Image.open(sys.argv[1])
	unscrambled_image = unscramble_image(image)

	image_2 = unscrambled_image.rotate(180)
	image_2.show()

	unscrambled_image = unscramble_image(image_2)

	unscrambled_image.show()

if __name__ == "__main__":
	# main()
	explore_image()
