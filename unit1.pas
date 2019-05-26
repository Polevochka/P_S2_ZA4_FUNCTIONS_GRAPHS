unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids, Functions, Jobs, Unit2;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckGroup1: TCheckGroup;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

  a, b:real;
  n: integer;
  Func: TFunc;
  Arfx: TArfx;

  // Процедура вычисления интеграла
  Integ: TInteg;

implementation

{$R *.lfm}

{Добавить столбец}
// Title - заголовок столбца(шапка)
// Добавляет одну колонку - столбец в таблицу
// со значениями из массива Arfx
procedure AddCol(Title: string; Arfx:TArfx);
var i: integer;
    s: string;
begin
  // Используем with чтобы меньше писать StringGrid1
  with Form1.StringGrid1 do
  begin
    // Увеличиваем число столбцов в таблице
    ColCount := ColCount +  1;
    // Надо увеличить и ширину таблицы под этот НОВЫЙ столбик
    width := width + 60;
    // Делаем Заголовок столбца
    Cells[ColCount -1, 0]:= Title;

    // Теперь можно заполнять НОВЫЙ столбик

    // Обходим все значения из масива Arfx
    for i:=1 to length(Arfx) do
    begin
      // Вещественное число из массива Arf надо перевести в строку s
      // Так как в StringGrid1 мы можем записывать только строки
      str(Arfx[i-1]:6:3, s);
      // i-ое значение из массива записывает в i-ую строку
      Cells[ColCount - 1, i]:= s;
    end;
  end;
end;

{ TForm1 }

{Особые действия перед началом работы}
procedure TForm1.FormCreate(Sender: TObject);
var i: integer;
begin
  // Для удобства сделаем так,
  // чтобы значения в edit-ах уже были при запуске проги
  Edit1.Text:='0';    // a
  Edit2.Text:= '10';  // b
  Edit3.Text:= '100'; // n

  // По приколу делаем таблицу невидимой - для красоты
  StringGrid1.Visible := False;

  // Режим Работы по умолчанию выбираем первый(нулевой) - Функции
  RadioGroup1.ItemIndex := 0;

  // Записываем ИМЕНА МЕТОДОВ взятия интеграла в меню интеграла
  // Число МЕТОДОВ зависит от размера массива
  // Поэтому если убрать один из него элемент
  // То надо и поменять определение и Массива САМИХ ПРОЦЕДУР интегрирования и Массива ИМЁН ПРОЦЕДУР методов интегрирования
  // А именно уменьшить верхний предел в модуле Jobs.pas
  // Иначе эта строчка вызовет ошибку, тк обращается к элементу которого нет
  for i:=0 to high(ArNameInteg) do
    RadioGroup2.Items.Add(ArNameInteg[i]);

  // Выбирам метод интегрирования по умолчанию 3 - метод ТРАПЕЦИЙ
  RadioGroup2.ItemIndex := 3;

  // Изначально у нас не выбран пункт интеграл
  // следовательно и меню интеграла не нужно показывать
  RadioGroup2.Visible:= False;

  // Записываем в комбобокс названия функция из модуля Functions
  for i:= 0 to high(ArNamef) do
    ComboBox1.Items.Add(ArNamef[i]);

  // По умоланию в комобобокс будет первая функция из массива
  // названий функция ArNamef
  ComboBox1.Text := ArNamef[0];
  Func := Arf[0];
end;


{В соответствии с выбранным режимом делаем СheckGroup видимой или невидимой}
procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  // Если выбран режим "функция"
  if RadioGroup1.ItemIndex = 0 then
    // Делаем меню для функции видимой
    CheckGroup1.Visible:= True
  else
    // Иначе меню для функции нам не нужно
    CheckGroup1.Visible:= False;
end;

{Получаем выбранную функцию из ComboBox}
// чтобы потом её использовать в вычислениях
procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  Func := Arf[ComboBox1.ItemIndex];
end;

{Вычислить}
procedure TForm1.Button1Click(Sender: TObject);
var dx:real;
    i, k: integer;
    tempMas: TArfx;
begin
  a:= StrToFloat(Edit1.Text);
  b:= StrToFloat(Edit2.Text);
  n:= StrToInt(Edit3.Text);

  // Проверка на корректность данных
  if (a>b) then
  begin
    // Говорим пользователю что он ошибся
    ShowMessage('a > b');
    // Выходим из процедуры досрочно
    // код после if выполняться не будет
    Exit;
  end;

  // Если это первый запуск, то таблицу надо сделать видимой,
  // так как в FormCreate мы её сделали невидимой
  StringGrid1.Visible := True;

  // Очищаем таблицу вдруг в ней что-то было
  StringGrid1.Clean;

  // При добавлении столбца AddCol
  // мы увеличиваем каждый раз размер таблицы
  StringGrid1.Width:= 0;

  // Вычисляем длину одного шага, тк в каждов режиме оно понадобится
  dx:= (b-a)/n;

  // Проверяем какая радиокнопка была выбрана
  case RadioGroup1.ItemIndex of
    // Функция
    0: begin

         // сразу выделяем нужное количество строк и столбцов
         // первая еденица - строка заголовка , а n+1 - под распределение
         StringGrid1.RowCount:=1 + n+1;

         // Нуль столбцов,
         // потому что при добавлении распределения AddCol
         // мы КАЖДЫЙ раз увеличиваем число столбцов
         // Причом этот шаг выполняем именно здесь, а не в общей части,
         // так как из-за задании Числа строк(чуть выше),
         // у нас автоматичски число столбцов становится = 1, а это означает лишний пустой столбец
         // При добавлениие нового с помощью Addcol
         StringGrid1.ColCount:= 0;

         // Задаём размер массива
         setlength(Arfx, n+1);

         {Выводим распределение x}
         // вычисляем распределение иксов
         for i:= 0 to n do
           Arfx[i]:= a + i*dx;

         // И выводим все 'x' в таблицу
         AddCol(' X ',Arfx);
         // Освобождаем память
         SetLength(Arfx, 0);

         {Проверяем какие из флажков были выбраны}

         // Выбрано F(x)
         if CheckGroup1.Checked[0] then
         begin
           SetLength(Arfx, n+1);
           Fotx(Func, a, b, n, Arfx);
           AddCol(CheckGroup1.items[0], Arfx);
           SetLength(Arfx, 0);
         end;

         // Выбрано F'(x)
         if CheckGroup1.Checked[1] then
         begin
           SetLength(Arfx, n+1);
           Df_dx(Func, a, b, n, Arfx);
           AddCol(CheckGroup1.items[1],Arfx);
           SetLength(Arfx, 0);
         end;

         // Выбрано F''(x)
         if CheckGroup1.Checked[2] then
         begin
           SetLength(Arfx, n+1);
           D2f_dx(Func, a, b, n, Arfx);
           AddCol(CheckGroup1.items[2],Arfx);
           SetLength(Arfx, 0);
         end;

         // Выбрано Интеграл
         if CheckGroup1.Checked[3] then
         begin
           // Получаем МЕТОД ВЗЯТИЯ интеграла из меню
           Integ:= ArInteg[RadioGroup2.ItemIndex];
           SetLength(Arfx, n+1); // Задаём размер получаемого массива
           Integ(Func, a, b, n, Arfx); // Вычисляем интеграл
           // Крч 'интеграл' тупо не влезало, поэтому 'Integ' азазаз
           AddCol('Integ',Arfx);
           SetLength(Arfx, 0);
         end;

         // Добавляем пространства для полосы прокрутки
         // причом если строки вмещаются в таблицу то пространство просто увеличится
         // без появления полосы прокрутки
         StringGrid1.Width:= StringGrid1.Width + 25;
       end;
    // Корни
    1: begin

         // Находим корни
         Roots(Func, a, b, n, Arfx, k);

         // Делаем проверку на существование корней, так как может быть задан интервал, в котором их нет
         if k = 0 then
           begin
             StringGrid1.Visible := False;
             ShowMessage('Корней нет');
           end
         else
         begin

           // Задаём размер вспомогательного массива
           setlength(tempMas, k);

           // Нужные значения для таблицы
           StringGrid1.RowCount:=1 + k;
           StringGrid1.ColCount:= 0;

           {Вычисляем X_пред - то есть x, стоящий ПЕРЕД корнем}

           // так как процедура возвращает толко САМИ КОРНИ, нужно проверить отдельные случаи
           // Если первый найденный корень = левой границе исследуемого отрезка [a,b]
           // То значение X_пред не должно выходить за границы [a,b]
           if Arfx[0] = a then tempMas[0]:= a // Поэтому X_пред для корня x=a становиться само значение 'a'
           else tempMas[0]:= Arfx[0]-dx; // В обычном случаее X_пред должен отличаться от корня на dx

           for i:=1 to k-1 do // Перебираем остальные корни, не включая первый, так как для него уже провели нужные действия
             tempMas[i]:= Arfx[i]-dx;

           // Выводим X_Пред
           AddCol('X_пред', tempMas);

           {Вычисляем F_пред}
           for i:=0 to k-1 do
             tempMas[i]:= Func(tempMas[i]);

           // Выводим F_пред
           AddCol('F_пред', tempMas);

           {Вычисляем X_сл - то есть x, стоящий ПОСЛЕ корня}
           // c последним корнем такая же беда как и с первым
           // Мы не должны выходить за границу заданного отрезка [a,b]
           if Arfx[k-1] = b then tempMas[k-1]:= b // Учитываемоба случая
           else tempMas[k-1]:= Arfx[k-1] + dx;

           for i:=0 to k-2 do // Перебираем все остальные корни, уже без последнего, так как его уже проверили
             tempMas[i]:= Arfx[i] + dx;

           // Выводим X_сл
           AddCol('X_сл', tempMas);

           {Вычисляем F_сл}
           for i:=0 to k-1 do
             tempMas[i]:= Func(tempMas[i]);

           // Выводим F_сл
           AddCol('F_сл', tempMas);

           // Освобождаем память
           setlength(tempMas, 0);
         end;
       end;
    // Экстремум
    2: begin

         // Находим екстремумы
         Extrems(Func, a,b, n, Arfx, k);
         // Задаём размер вспомогательного массива
         setlength(tempMas, k);

         StringGrid1.RowCount:=1 + k;
         StringGrid1.ColCount:= 0;

         {Координаты x стоящих ПЕРЕД точками экстремума}
         // Вычисляем X_пред
         for i:=0 to k-1 do
         begin
           tempMas[i]:= Arfx[i] -dx;
         end;
         // Выводим X_пред
         AddCol('X_пред', tempMas);

         {Значения функции точкек стоящих ПЕРЕД точками экстремума}
         // Вычисляем F_пред
         for i:=0 to k-1 do
         begin
           tempMas[i]:= Func(tempMas[i]);
         end;
         // Выводим F_пред
         AddCol('F_пред', tempMas);

         {Координаты x стоящих ПОСЛЕ точками экстремума}
         // Вычисляем X_сл
         for i:=0 to k-1 do
         begin
           tempMas[i]:= Arfx[i] + dx;
         end;
         // Выводим X_сл
         AddCol('X_сл', tempMas);

         {Значения функции точкек стоящих ПОСЛЕ точками экстремума}
         // Вычисляем F_сл
         for i:=0 to k-1 do
         begin
           tempMas[i]:= Func(tempMas[i]);
         end;
         // Выводим F_сл
         AddCol('F_сл', tempMas);

         {Значение функции в точках экстремума}
         // F_экстр
         for i:=0 to k-1 do
         begin
           tempMas[i]:= Func(Arfx[i]);
         end;
         // Выводим F_экстр
         AddCol('F_экстр', tempMas);

         // Освобождаем память
         setlength(tempMas, 0);
       end;

  end;
end;

{Выход}
procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

{Построить график}
procedure TForm1.Button3Click(Sender: TObject);
begin
  // Юзер может не нажать на кнопку 'Вычислить',
  // а без этой кнопки у переменных  a, b, n - нет значений
  a:= StrToFloat(Edit1.Text);
  b:= StrToFloat(Edit2.Text);
  n:= StrToInt(Edit3.Text);

  // Проверка на корректность данных
  if (a>b) then
  begin
    // Говорим пользователю что он ошибся
    ShowMessage('a > b');
    // Выходим из процедуры досрочно
    // код после if выполняться не будет
    Exit;
  end;

  // Показываем окно с графиком
  Form2.show;
end;

{Вызиывается если нажали на Checkgroup1}
//там где F(x), F'(x), F''(x), Интеграл
procedure TForm1.CheckGroup1ItemClick(Sender: TObject; Index: integer);
begin
  // Если выбран интеграл
  if Index = 3 then
    // Проверяем нам надо показать методы интегрирования или убрать их
    // Если окно интегралов невидимое, то его надо ВКЛЮЧИТЬ
    if RadioGroup2.Visible = False then
      // Делаем выдимым окно метода интеграла
      RadioGroup2.Visible:= True
    else
      // иначе ВЫКЛЮЧАЕМ
      RadioGroup2.Visible:= False;
end;


end.
