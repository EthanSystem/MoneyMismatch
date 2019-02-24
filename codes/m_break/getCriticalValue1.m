function cv=getCriticalValue1(signif,eps1)

%input of the critical values of the supF(k) test

if eps1 == .05
    cv=zeros(10,10);
    
    if signif == 1;
        cv=[
            8.02   7.87   7.07   6.61   6.14   5.74   5.40   5.09   4.81;
            11.02  10.48   9.61   8.99   8.50   8.06   7.66   7.32   7.01;
            13.43  12.73  11.76  11.04  10.49  10.02   9.59   9.21   8.86;
            15.53  14.65  13.63  12.91  12.33  11.79  11.34  10.93  10.55;
            17.42  16.45  15.44  14.69  14.05  13.51  13.02  12.59  12.18;
            19.38  18.15  17.17  16.39  15.74  15.18  14.63  14.18  13.74;
            21.23  19.93  18.75  17.98  17.28  16.69  16.16  15.69  15.24;
            22.92  21.56  20.43  19.58  18.84  18.21  17.69  17.19  16.70;
            24.75  23.15  21.98  21.12  20.37  19.72  19.13  18.58  18.09;
            26.13  24.70  23.48  22.57  21.83  21.16  20.57  20.03  19.55];
        
    end
    if signif == 2
        cv=[
            9.63   8.78   7.85   7.21   6.69   6.23   5.86   5.51   5.20;
            12.89  11.60  10.46   9.71   9.12   8.65   8.19   7.79   7.46;
            15.37  13.84  12.64  11.83  11.15  10.61  10.14   9.71   9.32;
            17.60  15.84  14.63  13.71  12.99  12.42  11.91  11.49  11.04;
            19.50  17.60  16.40  15.52  14.79  14.19  13.63  13.16  12.70;
            21.59  19.61  18.23  17.27  16.50  15.86  15.29  14.77  14.30;
            23.50  21.30  19.83  18.91  18.10  17.43  16.83  16.28  15.79;
            25.22  23.03  21.48  20.46  19.66  18.97  18.37  17.80  17.30;
            27.08  24.55  23.16  22.08  21.22  20.49  19.90  19.29  18.79;
            28.49  26.17  24.59  23.59  22.71  21.93  21.34  20.74  20.17];
        
    end
    if signif == 3
        cv=[
            11.17   9.81   8.52   7.79   7.22   6.70   6.27   5.92   5.56;
            14.53  12.64  11.20  10.29   9.69   9.10   8.64   8.18   7.80;
            17.17  14.91  13.44  12.49  11.75  11.13  10.62  10.14   9.72;
            19.35  16.85  15.44  14.43  13.64  13.01  12.46  11.94  11.49;
            21.47  18.75  17.26  16.13  15.40  14.75  14.19  13.66  13.17;
            23.73  20.80  19.15  18.07  17.21  16.49  15.84  15.29  14.78;
            25.23  22.54  20.85  19.68  18.79  18.03  17.38  16.79  16.31;
            27.21  24.20  22.41  21.29  20.39  19.63  18.98  18.34  17.78;
            29.13  25.92  24.14  22.97  21.98  21.28  20.59  19.98  19.39;
            30.67  27.52  25.69  24.47  23.45  22.71  21.95  21.34  20.79];
        
    end
    
    if signif == 4
        cv=[
            13.58  10.95   9.37   8.50   7.85   7.21   6.75   6.33   5.98;
            16.64  13.78  12.06  11.00  10.28   9.65   9.11   8.66   8.22;
            19.25  16.27  14.48  13.40  12.56  11.80  11.22  10.67  10.19;
            21.20  18.21  16.43  15.21  14.45  13.70  13.04  12.48  12.02;
            23.99  20.18  18.19  17.09  16.14  15.34  14.81  14.26  13.72;
            25.95  22.18  20.29  18.93  17.97  17.20  16.54  15.94  15.35;
            28.01  24.07  21.89  20.68  19.68  18.81  18.10  17.49  16.96;
            29.60  25.66  23.44  22.22  21.22  20.40  19.66  19.03  18.46;
            31.66  27.42  25.13  24.01  23.06  22.18  21.35  20.63  19.94;
            33.62  29.14  26.90  25.58  24.44  23.49  22.75  22.09  21.47];
    else
        
    end
end


if eps1 == .10
    
    cv=zeros(10,8);
    if signif == 1
        cv=[
            7.42    6.93    6.09    5.44    4.85    4.32    3.83    3.22;
            10.37    9.43    8.48    7.68    7.02    6.37    5.77    4.98;
            12.77   11.61   10.53    9.69    8.94    8.21    7.49    6.57;
            14.81   13.56   12.36   11.43   10.61    9.86    9.04    8.01;
            16.65   15.32   14.06   13.10   12.20   11.40   10.54    9.40;
            18.65   17.01   15.75   14.70   13.78   12.92   11.98   10.80;
            20.34   18.71   17.26   16.19   15.26   14.35   13.40   12.13;
            22.01   20.32   18.90   17.75   16.79   15.82   14.80   13.45;
            23.79   21.88   20.43   19.28   18.22   17.24   16.19   14.77;
            25.29   23.33   21.89   20.71   19.63   18.59   17.50   16.00 ];
    end
    if signif == 2
        cv=[
            9.10    7.92    6.84    6.03    5.37    4.80    4.23    3.58;
            12.25   10.58    9.29    8.37    7.62    6.90    6.21    5.41;
            14.60   12.82   11.46   10.41    9.59    8.80    8.01    7.03;
            16.76   14.72   13.30   12.25   11.29   10.42    9.58    8.46;
            18.68   16.50   15.07   13.93   13.00   12.10   11.16    9.96;
            20.76   18.32   16.81   15.67   14.65   13.68   12.63   11.34;
            22.62   20.04   18.45   17.19   16.14   15.11   14.09   12.71;
            24.34   21.69   20.01   18.74   17.66   16.65   15.54   14.07;
            26.20   23.36   21.63   20.32   19.19   18.09   16.89   15.40;
            27.64   24.87   23.11   21.79   20.58   19.47   18.29   16.70];
    end
    if signif == 3
        cv=[
            10.56    8.90    7.55    6.64    5.88    5.22    4.61    3.90;
            13.86   11.63   10.14    9.05    8.17    7.40    6.63    5.73;
            16.55   13.90   12.35   11.12   10.19    9.28    8.43    7.40;
            18.62   15.88   14.22   12.96   11.94   11.05   10.06    8.93;
            20.59   17.71   16.02   14.68   13.67   12.71   11.68   10.42;
            23.05   19.69   17.82   16.47   15.31   14.24   13.20   11.89;
            24.65   21.34   19.41   18.13   16.90   15.84   14.67   13.25;
            26.50   22.98   20.95   19.69   18.52   17.35   16.15   14.67;
            28.25   24.73   22.68   21.29   20.01   18.76   17.56   16.00;
            29.80   26.37   24.27   22.71   21.42   20.21   18.94   17.33];
    end
    if signif == 4
        cv=[
            13.00   10.14    8.42    7.31    6.48    5.74    5.05    4.28;
            16.19   12.90   11.12    9.87    8.84    8.01    7.18    6.18;
            18.72   15.38   13.38   11.97   10.93    9.94    8.99    7.85;
            20.75   17.24   15.30   13.93   12.78   11.67   10.64    9.47;
            23.12   18.93   16.91   15.61   14.42   13.31   12.30   11.00;
            25.50   21.15   19.04   17.48   16.19   15.11   13.88   12.55;
            27.19   22.97   20.68   19.14   17.81   16.59   15.43   13.92;
            29.01   24.51   22.40   20.68   19.41   18.08   16.83   15.30;
            30.81   26.30   23.95   22.33   20.88   19.56   18.35   16.79;
            32.80   28.24   25.63   23.83   22.32   21.04   19.73   18.10 ];
    end
end


if eps1 == .15
    
    cv=zeros(10,5);
    if signif == 1
        cv=[
            7.04    6.28    5.21    4.41    3.47;
            9.81    8.63    7.54    6.51    5.27;
            12.08   10.75    9.51    8.29    6.90;
            14.26   12.60   11.21    9.97    8.37;
            16.14   14.37   12.90   11.50    9.79;
            17.97   16.02   14.45   13.00   11.19;
            19.70   17.67   16.04   14.55   12.59;
            21.41   19.16   17.47   15.88   13.89;
            23.06   20.82   19.07   17.38   15.23;
            24.65   22.26   20.42   18.73   16.54 ];
    end
    if signif == 2;
        cv=[
            8.58    7.22    5.96    4.99    3.91;
            11.47    9.75    8.36    7.19    5.85;
            13.98   11.99   10.39    9.05    7.46;
            16.19   13.77   12.17   10.79    9.09;
            18.23   15.62   13.93   12.38   10.52;
            20.08   17.37   15.58   13.90   11.94;
            21.87   18.98   17.23   15.55   13.40;
            23.70   20.62   18.69   16.96   14.77;
            25.65   22.35   20.18   18.40   16.11;
            27.03   23.80   21.62   19.79   17.44 ];
    end
    if signif == 3
        cv=[
            10.18    8.14    6.72    5.51    4.34;
            12.96   10.75    9.15    7.81    6.38;
            15.76   13.13   11.23    9.72    8.03;
            18.13   14.99   13.06   11.55    9.66;
            19.95   16.92   14.98   13.25   11.21;
            22.15   18.62   16.50   14.68   12.63;
            24.20   20.40   18.25   16.41   14.18;
            25.77   21.97   19.71   17.91   15.52;
            27.69   23.68   21.28   19.29   16.88;
            29.27   24.99   22.74   20.81   18.26];
    end
    if signif == 4
        cv=[
            12.29    9.36    7.60    6.19    4.91;
            15.37   12.15   10.27    8.65    7.00;
            18.26   14.45   12.16   10.56    8.71;
            20.23   16.55   14.26   12.42   10.53;
            22.40   18.37   16.16   14.25   12.14;
            24.45   20.06   17.57   15.73   13.44;
            26.71   21.87   19.42   17.44   15.02;
            28.51   23.58   20.96   19.00   16.56;
            30.62   25.32   22.72   20.38   17.87;
            32.16   26.82   24.41   22.09   19.27];
    end
end


if eps1 == .20
    
    cv=zeros(10,3);
    if signif == 1
        cv=[
            6.72    5.59    4.37;
            9.37    7.91    6.43;
            11.59    9.93    8.21;
            13.72   11.70    9.90;
            15.51   13.46   11.50;
            17.39   15.05   12.91;
            19.11   16.67   14.46;
            20.86   18.16   15.88;
            22.38   19.71   17.30;
            23.95   21.13   18.65];
    end
    if signif == 2
        cv=[
            8.22    6.53    5.08;
            10.98    8.98    7.13;
            13.47   11.09    9.12;
            15.67   12.94   10.78;
            17.66   14.69   12.45;
            19.55   16.35   13.91;
            21.33   18.14   15.55;
            23.19   19.58   17.10;
            24.91   21.23   18.58;
            26.38   22.62   19.91];
    end
    if signif == 3
        cv=[
            9.77    7.49    5.73;
            12.59   10.00    7.92;
            15.28   12.25    9.91;
            17.67   14.11   11.66;
            19.51   15.96   13.49;
            21.47   17.66   14.97;
            23.36   19.41   16.56;
            25.26   20.94   18.03;
            26.96   22.69   19.51;
            28.62   24.04   20.96];
    end
    if signif == 4
        cv=[
            11.94    8.77    6.58;
            14.92   11.30    8.95;
            17.60   13.40   10.91;
            19.82   15.74   12.99;
            21.75   17.21   14.60;
            23.80   19.25   16.29;
            26.16   21.03   17.81;
            27.71   22.71   19.37;
            29.67   24.43   20.74;
            31.38   25.73   22.34];
    end
end

if eps1 == .25
    
    cv=zeros(10,2);
    if signif ==  1
        cv=[
            6.35    4.88;
            8.96    7.06;
            11.17    9.01;
            13.22   10.74;
            14.98   12.39;
            16.77   13.96;
            18.45   15.53;
            20.15   16.91;
            21.69   18.42;
            23.29   19.84];
    end
    if signif == 2;
        cv=[
            7.86    5.80;
            10.55    8.17;
            13.04   10.16;
            15.19   11.91;
            17.12   13.65;
            18.97   15.38;
            20.75   16.97;
            22.56   18.43;
            24.18   19.93;
            25.77   21.34];
    end
    if signif == 3
        cv=[
            9.32    6.69;
            12.21    9.16;
            14.66   11.22;
            17.04   13.00;
            18.96   14.86;
            20.93   16.53;
            22.85   18.25;
            24.56   19.68;
            26.31   21.38;
            27.80   22.79];
    end
    if signif == 4
        cv=[
            11.44    7.92;
            14.34   10.30;
            17.08   12.55;
            19.22   14.65;
            21.51   16.18;
            23.12   18.10;
            25.67   19.91;
            27.10   21.41;
            29.12   23.23;
            30.86   24.51];
    end
end
