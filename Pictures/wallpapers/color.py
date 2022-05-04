from pathlib import Path
import math
import networkx as nx
import pandas as pd
import sklearn as sk
from sklearn import cluster
import numpy as np
from scipy.spatial import ConvexHull
from PIL import Image, ImageDraw, ImageFont

def hex_to_rgb(hex_color):
    return (int(hex_color[1:3], 16), int(hex_color[3:5], 16), int(hex_color[5:7], 16))

def display_color(rgb):
    possible_colors = [(0,0,0), (255,255,255)]
    distance = {}
    for pc in possible_colors:
        distance[pc] = math.sqrt((rgb[0] - pc[0])**2 + (rgb[1] - pc[1])**2 + (rgb[2] - pc[2])**2)
    return max(distance, key=distance.get)

def generate_colors(pixels):
    """
    pixels: numpy ndarray with columns R G B, one row per pixel

    returns list of RGB tuples
    """
    # compute a convex hull and find the vertices
    hull = ConvexHull(pixels)
    verts = np.take(pixels, hull.vertices, axis=0)

    # Cluster down to eight clusters
    clusterer = cluster.KMeans()
    clusterer.fit(verts)
    return [
        f"#{c[0]:02X}{c[1]:02X}{c[2]:02X}"
        for c in np.around(clusterer.cluster_centers_).astype(int)
    ]


def main():
    path = Path(".")
    font = ImageFont.truetype("/usr/share/fonts/truetype/ubuntu/UbuntuMono-R.ttf", 16)

    for img in path.iterdir():
        if img.suffix != ".jpeg":
            print(f"Skipping {str(img)}, not JPEG\n")
            continue
        image = Image.open(str(img))
        pixels = np.array(list(image.getdata()), dtype=np.ubyte)
        colors = generate_colors(pixels)
        print(f"For image '{str(img)}':")
        print(colors)
        print()

        palatte = Image.new('RGB', (400, 400))
        for i, c in enumerate(colors):
            rgb = hex_to_rgb(c)
            palatte_d = ImageDraw.Draw(palatte)
            palatte_d.rectangle(xy=[0, i*50, 400, (i+1)*50], fill=rgb, outline=rgb)
            palatte_d.text(xy=(200, i*50 + 25), text=c, fill=display_color(rgb), anchor="mm", font=font)

        palatte.save(f"{img.stem}_palatte.jpeg")
        


if __name__ == "__main__":
    main()
