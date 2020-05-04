def split(subway):
    # symbol = [',',]
    station = subway.split(', ')
    # print(station)
    for x in range(len(station) - 1):
        print('next('+ station[x].lower().replace(' ', '_') + ', ' + station[x+1].lower().replace(' ', '_') + ', line_12).')

if __name__ == '__main__':
    subway = 'Tláhuac, Tlaltenco, Zapotitlán, Nopalera, Olivos, Tezonco, Periférico Oriente, Calle 11, Lomas Estrella, San Andrés Tomatlán, Culhuacán, Atlalilco, Mexicaltzingo, Ermita, Eje Central, Parque de los Venados, Zapata, Hospital 20 de Noviembre, Insurgentes Sur, Mixcoac'
    split(subway)
