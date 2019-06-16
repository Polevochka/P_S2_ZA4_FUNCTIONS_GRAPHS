unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Image1: TImage;
    procedure FormShow(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
  private

  public

  end;

var
  Form2: TForm2;

implementation
// чтобы не было зацикленности именно после implementation,
// т.к. Unit1 также подключает Unit2
Uses Unit1, Jobs;

Type
  // Тип массива точек для передачи в функции
  TMasPoints = array of TPoint;

var Xn, Yn, Xk, Yk: integer; // Область Image1 выделенная под рисование графика
    Ymin, Ymax: real; // Минимуы максимумы функций
    mx, my: real; // Коэффициенты для отображения графика на ВСЁМ Image1

{$R *.lfm}

{ TForm2 }

// Процедура поиска минимального и максимального ЗНАЧЕНИЯ функции
// Func - появилась из первого модуля Unit1
// Arfx - получен процедурой FotX(Модуль Jobs.pas) - распределение игреков на нужном нам интервале
procedure PosMinMax(Arfx: TArfx; var Ymin: real; var Ymax: real);
var i: integer;
    Y: real; // Чтобы было более понятно что такое Arfx[i]
begin
  Ymin:= Arfx[0];
  Ymax:=  Arfx[0];
  for i:=0 to high(Arfx) do // Перебираем все игреки
    begin
      Y:= Arfx[i];
      if Y < Ymin then Ymin:= Y
      else if Y > Ymax then Ymax:= Y;
    end;
end;


{Возвращает расстояние между двумя точками}
function GetDistance(p1: Tpoint; p2: Tpoint): real;
begin
  // То что мы присваим имени функции является результатом её работы
  // Не надо бояться выражения ниже - это обычная формула из школьной геометрии
  // Для нахождения расстояния между двумя точками
  //      _______________________
  // r = √ (x1-x2)^2 + (y1-y2)^2
  GetDistance:=sqrt(sqr(p1.x - p2.x) + sqr(p1.y - p2.y));
end;

{Пересечения графика с прямыми вида y=L, где L=const - ГОРИЗОНТАЛЬНЫЕ линии}
// GraficPoints - массив коордиат графика
// ResPoints - массив точек пересечения графика с прямой y = Yg
procedure CrossingsY(GraficPoints: TMasPoints; var ResPoints: TMasPoints; Yg: integer);
var k, i: integer;
    p1, p2: TPoint; // Две точки графика между которыми прямая y = Yg
    p: TPoint;      // точка пересечения 'графика' И 'прямой y = Yg'
    EPS: real;      // точность с которой находим точки
begin

  // Считаем что в радиусе EPS  от НАЙДЕННОЙ точки пересечения не может быть
  // других точек пересечения с прямой y=Yg.
  // Делаем это для того, чтобы предупредить НАЛОЖЕНИЕ точек персечения
  // Так как график строится не точно, и они могут НАКЛАДЫВАТЬСЯ
  EPS := 12;

  k:=0; // счётчик найденных точек пересечения
  // Обходим точки графика
  for i:=0 to high(GraficPoints) do
  begin

    // Так как для соединения точек графика мы используем
    // Image1.Canvas.Polyline(GraficPoints);
    // то две соседние точки соединяются просто прямой линией
    // И чтобы найти точку пересечения с этой линией надо найти две точки
    // что по РАЗНЫЕ стороны от прямой y=Yg - как с корнями

    // рассмотрим две СОСЕДНИЕ точки ГРАФИКА
    p1 := GraficPoints[i];
    p2 := GraficPoints[i+1];

    // Если это ТЕ САМЫ две точки ГРАФИКА между которыми есть прямая y=Yg
    // То они должны быть по РАЗНЫЕ стороны от этой прямой
    //     (p1 - ниже И p2 выше)    или    (p1 - выше И p2 - ниже)
    if (p1.y <= Yg) and (p2.y >= Yg) or (p1.y >= Yg) and (p2.y <= Yg) then
    begin

      // Здесь мы НАШЛИ две точки - между которыми еcть точка пересечения
      // 'графика' И 'прямой y=Yg'

      // Используя всем известное уровнение
      // прямой, проходящей через две точки p1 и p2
      // можно легко найти абциссу точки пересечения
      p.x := round((Yg-p1.y)*(p2.x-p1.x)/(p2.y-p1.y)) + p1.x;
      // а ордината этой точки уже известна
      p.y := Yg;

      // Если это ПЕРВАЯ НАЙДЕННАЯ ТОЧКА (k=0)
      //        ИЛИ
      // если УЖЕ находили точки пересечения (k > 0)
      // Делаем проверку на наложение точек
      //        Предыдущая найденна точка ResPoints[k-1]
      //        ДОЛЖНА быть на радиус EPS ДАЛЬШЕ от новой
      //        - чтобы точки НЕ НАКЛАДЫВАЛИСЬ
      //        (GetDistance(p, ResPoints[k-1] > EPS)
      if (k=0) or ((k > 0) and (GetDistance(p, ResPoints[k-1]) > EPS)) then
      begin
        // Добавляем новую точку в массив

        k := k+1; // Увеличиваем число элементов в получаемом массиве точек пересечения
        setlength(ResPoints, k); // Задаём размер массива, т.к. k- это просто счётчик
        ResPoints[k-1]:= p; // Записываем в массив точку пересечения
      end;
    end;
  end;
end;

{Пересечения графика с прямыми вида x=L, где L=const - ВЕРТИКАЛЬНЫЕ линии}
// GraficPoints - массив коордиат графика
// ResPoints - массив точек пересечения графика с прямой x = Xg
procedure CrossingsX(GraficPoints: TMasPoints; var ResPoints: TMasPoints; Xg: integer);
var k, i: integer;
    p1, p2: TPoint; // Две точки графика между которыми прямая y = Yg
    p: TPoint;      // точка пересечения 'графика' И 'прямой y = Yg'
    EPS: real;      // точность с которой находим точки
begin
  // Считаем что в радиусе EPS  от НАЙДЕННОЙ точки пересечения не может быть
  // других точек пересечения с прямой y=Yg.
  // Делаем это для того, чтобы предупредить НАЛОЖЕНИЕ точек персечения
  // Так как график строится не точно, и они могут НАКЛАДЫВАТЬСЯ
  EPS := 12;

  // Делаем все также как и с CrossingsY только для вертикальных линий
  k:=0;// счётчик найденных точек пересечения
  // Обходим точки графика
  for i:=0 to high(GraficPoints) do
  begin
    // Так как для соединения точек графика мы используем
    // Image1.Canvas.Polyline(GraficPoints);
    // то две соседние точки соединяются просто прямой линией
    // И чтобы найти точку пересечения с этой линией надо найти две точки
    // что по РАЗНЫЕ стороны от прямой x=Xg - как с корнями

    // рассмотрим две СОСЕДНИЕ точки ГРАФИКА
    p1 := GraficPoints[i];
    p2 := GraficPoints[i+1];

    // Если это ТЕ САМЫ две точки ГРАФИКА между которыми есть прямая y=Yg
    // То они должны быть по РАЗНЫЕ стороны от этой прямой
    //     (p1 - левее И p2 - правее)    или    (p1 - правее И p2 - левее)
    if (p1.x <= Xg) and (p2.x >= Xg) or  (p1.x >= Xg) and (p2.x <= Xg) then
    begin
      // Здесь мы НАШЛИ две точки - между которыми еcть точка пересечения
      // 'графика' И 'прямой x=Xg'

      // Ординату получаем из уравнения прямой проходящей через две точки
      p.y := round((Xg-p1.x)*(p2.y-p1.y)/(p2.x-p1.x)) + p1.y;
      // Абцисса уже дана
      p.x := Xg;

      // Если это ПЕРВАЯ НАЙДЕННАЯ ТОЧКА (k=0)
      //        ИЛИ
      // если УЖЕ находили точки пересечения (k > 0)
      // Делаем проверку на наложение точек
      //        Предыдущая найденна точка ResPoints[k-1]
      //        ДОЛЖНА быть на радиус EPS ДАЛЬШЕ от новой
      //        - чтобы точки НЕ НАКЛАДЫВАЛИСЬ
      //        (GetDistance(p, ResPoints[k-1] > EPS)
      if (k=0) or ((k > 0) and (GetDistance(p, ResPoints[k-1]) > EPS)) then
      begin
        // Добавляем новую точку в массив

        k := k+1; // Увеличиваем число элементов в получаемом массиве точек пересечения
        setlength(ResPoints, k); // Задаём размер массива, т.к. k- это просто счётчик
        ResPoints[k-1]:= p; // Записываем в массив точку пересечения
      end;
    end;
  end;
end;

{Вызывается при отображения Form2}
procedure TForm2.FormShow(Sender: TObject);
var GraficPoints : TMasPoints; // Массив точек графика
    CrossPoints: TMasPoints;   // массив точек пересечения
    i,j: integer;
    x :real; // Для перебора Иксов
    Ndx, Ndy: integer;  // Для координатной сетки
    Xg, Yg: integer;    // Графические координаты графика
    DXg, DYg: integer;
    Xf, Yf: real;
    dx, dy: real;
    sx, sy: string;
    p: Tpoint;  // используется для рисования кружков, квадратиков треугольников
begin

  // Делаем надпись для графика в заголовке окна
  Form2.Caption:= 'График 1-ой ПРОИЗВОДНОЙ  |  y=' + Form1.ComboBox1.Items[Form1.ComboBox1.ItemIndex];
  //или
  //Form2.Caption:= 'График 2-ой ПРОИЗВОДНОЙ  |  y=' + Form1.ComboBox1.Items[Form1.ComboBox1.ItemIndex];


  // Cтираем всё что было на холсте
  Image1.Canvas.Brush.Color:= clWhite;
  Image1.Canvas.FillRect(Rect(0, 0, Image1.Width, Image1.Height));

  // 1) Задаёмся размерами области графика
  Xn:= 50;
  Yn:= 20;
  Xk:= Image1.Width - 30;
  Yk:= Image1.Height - 30;

  // 2) Определение Ymin, Ymax
  setlength(Arfx, n + 1);

  // Было
  // FotX(Func, a, b, n, Arfx); // Получаем распределение y - игреков
  // Заменяем на
  // Получаем распределение точек графика прозводной
  Df_dx(Func, a, b, n, Arfx);

  // Если надо построить график ВТОРОЙ производной, то Df_dx заменяем на
  //D2f_dx(Func, a, b, n, Arfx);

  PosMinMax(Arfx, Ymin, Ymax);

  // 3) Операция масштабирования - вычисление масштабных коэффициентов
  mx:= (Xk - Xn)/(b-a);
  my:= (Yk - Yn)/(Ymax - Ymin);

  // 4) Вычисление значений координат расчётных точек в графической системе координат
  setlength(GraficPoints, n+1); // Задаём размер массива точек
  dx:= (b-a)/n;             // Находим Шаг
  // Заполняем этот массив точками графика
  for i:= 0 to high(GraficPoints) do
    begin
      x:= a + i*dx; //вычисляем нужную точку
      // Переходим к графическим координатам
      GraficPoints[i].x:= Xn + Round(mx*(x-a));
      GraficPoints[i].y:= Yn + Round(my*(Ymax-Arfx[i])); // нет смысла использовать Func(x) так как Игреки(Y) уже были посчитаны на 2)-ом шаге
    end;

  // 5) Нанесение координатной сетки оцифровка осей
  // В реальных координатах
  Ndx:= 10; // Число интервалов между линиями по x
  Ndy:= 10; // Число интервалов между линиями по Y

  // Шаги по x и y в ГРАФИЧЕСКИХ координатах
  DXg:= (Xk - Xn) div Ndx;
  DYg:= (Yk - Yn) div Ndy;
  // Шаги по x и y в РЕАЛЬНЫХ координатах
  dx:= (b-a)/Ndx;
  dy:= (Ymax-Ymin)/Ndy;

  {Делаем полосы параллельные OY}

  // Полосы должны быть чёрного цвета
  Image1.Canvas.Pen.Color:= clBlack;
  Image1.canvas.Pen.Width:=1; // задаём жирность линии
  for i:= 0 to Ndx do
  begin
    // Рисуем одну вертикальную линию
    Xg := Xn + i * DXg;
    Image1.Canvas.MoveTo(Xg, Yn);
    Image1.Canvas.LineTo(Xg, Yk);

    // Делаем подпись для неё
    Xf:= a + i*dx;
    str(Xf:5:2,sx);
    Image1.Canvas.TextOut(Xg - Image1.Canvas.TextWidth(sx) div 2, yk + 10, sx);
  end;

  {Делаем полосы параллельные OX}

  // Полосы должны быть чёрного цвета
  Image1.Canvas.Pen.Color:= clBlack;
  Image1.canvas.Pen.Width:=1; // задаём размер линии - жирность
  for i:= 0 to Ndy do
  begin
    // Рисуем одну горизонтальную линию
    Yg := Yk - i * DYg;
    Image1.Canvas.MoveTo(Xn, Yg);
    Image1.Canvas.LineTo(Xk, Yg);

    // Делаем подпись для неё
    Yf:= Ymin +  i*dy;
    str(Yf:5:2,sy);
    Image1.Canvas.TextOut(Xn - (Image1.Canvas.TextWidth(sy) + 10) ,Yg - Image1.Canvas.TextHeight(sy) div 2, sy);

  end;

  // Рисуем График
  Image1.Canvas.Pen.Color:= clBlue; // Рисуем его синим цветом шоб выделялся
  Image1.canvas.Pen.Width:=2; // Сделаем его жирнее сетки
  Image1.Canvas.Polyline(GraficPoints);

  // Преподаватель может помимо построения графика производной
  // поставить точки пересечения графика с сеткой

  // p - точка которую надо поставить на графике

  // Так добавляем точку ТРЕУГОЛЬНИКОМ
  {
            Image1.Canvas.MoveTo(p.x, p.y+5);
            Image1.Canvas.LineTo(p.x-5, p.y-5);
            Image1.Canvas.LineTo(p.x+5, p.y-5);
            Image1.Canvas.LineTo(p.x, p.y+5);
  }

  // так делаем точку КВАДРАТИКОМ
  {
           Image1.Canvas.Rectangle(p.x-3, p.y-3, p.x+3, p.y+3);
  }

  // Так делем точку КРУЖОЧКОМ
  {
           Image1.Canvas.Ellipse(p.x-3, p.y-3, p.x+3, p.y+3);
  }

  // Преподаватель может выбрать на своё усмотрение каким способом
  // вы должны поставить точку
  // Для краткости кода я буду ставить точку КРУЖКОМ - но вас могут попросить
  // сделать КВАДРАТ или ТРЕУГОЛЬНИК
  // Как их делать было описано выше
  // Ниже представлены варианты защиты


  {Рисуем точки пересечения Графика с ВЕРТЕКАЛЬНЫМИ линиями сетки}
  {
  Image1.Canvas.Pen.Color:= clRed;
  Image1.Canvas.Pen.Width:=1;
  for i:= 0 to Ndx do
  begin
    // Получаем положение i-ой линии только в ГРАФИЧЕСКИХ координатах
    Xg := Xn + i * DXg;
    // Находим точки пересечения с ВЕРТИКАЛЬНОЙ прямой Xg
    CrossingsX(GraficPoints, CrossPoints, Xg);
    // Перебираем их в циклке
    for j:= 0 to high(CrossPoints) do
    begin
      p:= CrossPoints[j];
      // Ставим эту самую точку
      Image1.Canvas.Ellipse(p.x-3, p.y-3, p.x+3, p.y+3);
    end;
  end;
  }

  {Рисуем точки пересечения графика с ГОРИЗОНТАЛЬНЫМИ линиями сетки}
  {
  Image1.Canvas.Pen.Color:= clRed;
  Image1.Canvas.Pen.Width:=1;
  for i:= 0 to Ndy do
  begin
    // Получаем положение i-ой линии только в ГРАФИЧЕСКИХ координатах
    Yg := Yk - i * DYg;

    // Находим точки пересечения с ГОРИЗОНТАЛЬНОЙ прямой Yg
    CrossingsY(GraficPoints, CrossPoints, Yg);

    // Перебираем их в циклке
    for j:= 0 to high(CrossPoints) do
    begin
      p:= CrossPoints[j];
      // Ставим эту самую точку
      Image1.Canvas.Ellipse(p.x-3, p.y-3, p.x+3, p.y+3);
    end;
  end;
  }


end;

{Координаты под курсором}
// Вызывается когда мы останавливаем где-нибудь курсор
// Служит для того чтобы показать координаты точки, на которую указывает курсор
procedure TForm2.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var Xf, Yf: real;
    sx, sy: string;
begin
  // Возвращаемся от графических координат к реальным
  Xf:= a + (X - Xn)/mx;
  Yf:= Ymax - (Y - Yn)/my;

  // Переводим всё в строки
  str(Xf:5:2, sx);
  str(Yf:5:2, sy);

  // То что будет отображаться около курсора
  // #13 - переход на другую строку
  Hint:= 'x='+sx + #13 +'y='+ sy;
end;


end.

