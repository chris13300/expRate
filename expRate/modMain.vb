Module modMain
    Public entete As String
    Public epd As String
    Public longueurMax As Integer
    Public experienceRate As Integer
    Public nbOuverturesTraitees As Integer
    Public moteur_court As String

    Sub Main()
        Dim chaine As String, tabChaine() As String, tabTmp() As String, i As Integer
        Dim fichierINI As String, fichierEXP As String, moteurEXP As String, fichierLOG As String
        Dim modeReverse As Boolean, tabOuvertures() As String, indexOuvertures As Integer, nbOuvertures As Integer

        Console.Title = My.Computer.Name

        If My.Computer.FileSystem.GetFileInfo(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) & "Documents\Visual Studio 2013\Projects\expRate\expRate\bin\Debug\expRate.exe").LastWriteTime > My.Computer.FileSystem.GetFileInfo(My.Application.Info.AssemblyName & ".exe").LastWriteTime Then
            MsgBox("Il existe une version plus récente de ce programme !", MsgBoxStyle.Information)
            End
        End If

        fichierINI = My.Computer.Name & ".ini"
        moteurEXP = "D:\JEUX\ARENA CHESS 3.5.1\Engines\Eman\20T Eman 8.20 x64 PCNT.exe"
        fichierEXP = "D:\JEUX\ARENA CHESS 3.5.1\Engines\Eman\Eman.exp"
        If My.Computer.Name = "BOIS" Or My.Computer.Name = "HTPC" Or My.Computer.Name = "TOUR-COURTOISIE" Then
            moteurEXP = "D:\JEUX\ARENA CHESS 3.5.1\Engines\Eman\20T Eman 8.20 x64 BMI2.exe"
            fichierEXP = "D:\JEUX\ARENA CHESS 3.5.1\Engines\Eman\Eman.exp"
        ElseIf My.Computer.Name = "BUREAU" Or My.Computer.Name = "WORKSTATION" Then
            moteurEXP = "E:\JEUX\ARENA CHESS 3.5.1\Engines\Eman\20T Eman 8.20 x64 BMI2.exe"
            fichierEXP = "E:\JEUX\ARENA CHESS 3.5.1\Engines\Eman\Eman.exp"
        End If

        If My.Computer.FileSystem.FileExists(fichierINI) Then
            chaine = My.Computer.FileSystem.ReadAllText(fichierINI)
            If chaine <> "" And InStr(chaine, vbCrLf) > 0 Then
                tabChaine = Split(chaine, vbCrLf)
                For i = 0 To UBound(tabChaine)
                    If tabChaine(i) <> "" And InStr(tabChaine(i), " = ") > 0 Then
                        tabTmp = Split(tabChaine(i), " = ")
                        If tabTmp(0) <> "" And tabTmp(1) <> "" Then
                            If InStr(tabTmp(1), "//") > 0 Then
                                tabTmp(1) = Trim(gauche(tabTmp(1), tabTmp(1).IndexOf("//") - 1))
                            End If
                            Select Case tabTmp(0)
                                Case "moteurEXP"
                                    moteurEXP = Replace(tabTmp(1), """", "")
                                Case "fichierEXP"
                                    fichierEXP = Replace(tabTmp(1), """", "")
                                Case Else

                            End Select
                        End If
                    End If
                Next
            End If
        End If
        My.Computer.FileSystem.WriteAllText(fichierINI, "moteurEXP = " & moteurEXP & vbCrLf _
                                                      & "fichierEXP = " & fichierEXP & vbCrLf, False)

        If Not expV2(fichierEXP) Then
            MsgBox(nomFichier(fichierEXP) & " <> experience format v2 !?", MsgBoxStyle.Exclamation)
            End
        End If

        moteur_court = nomFichier(moteurEXP)

        chaine = Replace(Command(), """", "")
        If chaine = "" Then
            Console.WriteLine("Which position ?")
            Console.WriteLine("(enter an UCI string or leave blank for the default startpos)")
            chaine = Trim(Console.ReadLine)
        End If
        
        longueurMax = 0
        modeReverse = False
        If MsgBox("Normal (Yes) or Reverse (No) ?", MsgBoxStyle.YesNo) = MsgBoxResult.No Then
            modeReverse = True
        End If
        If My.Computer.FileSystem.FileExists(chaine) Then
            tabOuvertures = listerPositions(chaine)
            nbOuvertures = tabOuvertures.Length - 1

            fichierLOG = Replace(nomFichier(chaine), ".pgn", " (" & Replace(moteur_court, ".exe", "") & ").log")
            If My.Computer.FileSystem.FileExists(fichierLOG) Then
                My.Computer.FileSystem.DeleteFile(fichierLOG)
            End If

            Console.Write(vbCrLf & "Loading " & moteur_court & "... ")
            chargerMoteur(moteurEXP, fichierEXP)
            Console.WriteLine("OK" & vbCrLf)

            experienceRate = 0
            nbOuverturesTraitees = 0
            For indexOuvertures = 1 To nbOuvertures
                If tabOuvertures(indexOuvertures - 1) <> "" Then
                    tabTmp = Split(tauxExperience(tabOuvertures(indexOuvertures - 1), modeReverse), ";")
                    My.Computer.FileSystem.WriteAllText(fichierLOG, tabTmp(0) & StrDup(longueurMax - Len(tabTmp(0)), " ") & " : " & tabTmp(1) & vbCrLf, True)
                    Console.Title = My.Computer.Name & " : " & "expRate @ " & Format(experienceRate / nbOuverturesTraitees, "0%") & ", opening @ " & Format(indexOuvertures / nbOuvertures, "0%")
                End If
            Next

            My.Computer.FileSystem.WriteAllText(fichierLOG, vbCrLf & "Experience Rate : " & Format(experienceRate / nbOuverturesTraitees, "0%") & vbCrLf, True)

            'décharger le moteur
            dechargerMoteur()
        Else
            Console.Write(vbCrLf & "Loading " & moteur_court & "... ")
            chargerMoteur(moteurEXP, fichierEXP)
            Console.WriteLine("OK" & vbCrLf)

            Console.WriteLine(entete)

            Console.Title = "searching in " & nomFichier(fichierEXP)
            tauxExperience(chaine, modeReverse)

            'décharger le moteur
            dechargerMoteur()
        End If

        Console.WriteLine("Press ENTER to close the window.")
        Console.ReadLine()
    End Sub

    Private Function listerPositions(fichierPGN As String) As String()
        Dim chaine As String, tabChaine() As String, i As Integer
        Dim fichierUCI As String

        fichierUCI = Replace(fichierPGN, ".pgn", "_uci.pgn")
        If My.Computer.FileSystem.FileExists(fichierUCI) Then
            My.Computer.FileSystem.DeleteFile(fichierUCI)
        End If
        pgnUCI("pgn-extract.exe", fichierPGN, "_uci")
        tabChaine = Split(My.Computer.FileSystem.ReadAllText(fichierUCI), vbCrLf)

        chaine = ""

        For i = 0 To UBound(tabChaine)
            If tabChaine(i) <> "" Then
                If InStr(tabChaine(i), "[") = 0 And InStr(tabChaine(i), "]") = 0 Then
                    tabChaine(i) = Trim(Replace(tabChaine(i), "*", ""))
                    tabChaine(i) = Trim(Replace(tabChaine(i), "1-0", ""))
                    tabChaine(i) = Trim(Replace(tabChaine(i), "0-1", ""))
                    tabChaine(i) = Trim(Replace(tabChaine(i), "1/2-1/2", ""))
                    chaine = chaine & Trim(tabChaine(i)) & vbCrLf
                    If Len(Trim(tabChaine(i))) > longueurMax Then
                        longueurMax = Len(Trim(tabChaine(i)))
                    End If
                End If
            End If
        Next

        If My.Computer.FileSystem.FileExists(fichierUCI) Then
            My.Computer.FileSystem.DeleteFile(fichierUCI)
        End If

        Return Split(chaine, vbCrLf)
    End Function

    Private Function tauxExperience(position As String, Optional modeReverse As Boolean = False) As String
        Dim chaine As String, tabChaine() As String, tabTmp() As String, i As Integer, positionVide As Boolean
        Dim departEPD As String, departEPDMem As String, coup As String, horizon As Integer
        Dim tabPositions(1000000) As String, tabPositionsMem As String, indexPosition As Integer, nbCoupsVides As Integer, nbPositions As Integer
        Dim total As Integer, totalMem As Integer, moyenne As Integer, moyenneMem As Integer, visite As Integer, profondeur As Integer, offsetPosition As Integer, nbPositionsMem As Integer

        departEPDMem = ""
        tabPositionsMem = ""
        nbPositionsMem = 0
        totalMem = 0
        moyenneMem = 0

mode_reverse:

        horizon = 80 'nombre de coups maxi

        tabPositions(0) = position
        indexPosition = 0

        departEPD = ""

        positionVide = False

        visite = 0
        total = 0

        profondeur = 0
        moyenne = 0

        offsetPosition = 0
        nbCoupsVides = 0
        nbPositions = 0

        Do
            position = tabPositions(indexPosition)
            chaine = expListe(position)
            If indexPosition = 0 Then
                If tabPositions(0) <> "" Then
                    departEPD = epd
                Else
                    departEPD = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
                End If
            End If

            If chaine = "" And Not positionVide Then
                chaine = listerCoupsLegaux(position)
                positionVide = True
                nbCoupsVides = 0
            End If

            If chaine <> "" Then
                tabChaine = Split(chaine, vbCrLf)
                For i = 0 To UBound(tabChaine)
                    If tabChaine(i) <> "" And InStr(tabChaine(i), "mate", CompareMethod.Text) = 0 Then
                        tabTmp = Split(Replace(tabChaine(i), ":", ","), ",")

                        coup = Trim(tabTmp(1))
                        chaine = Trim(position & " " & coup)

                        visite = CInt(Trim(tabTmp(7)))
                        profondeur = CInt(Trim(tabTmp(3)))

                        If positionVide And profondeur = 0 And visite = 0 Then
                            nbCoupsVides = nbCoupsVides + 1
                        Else
                            total = total + visite
                            moyenne = moyenne + profondeur
                        End If

                        nbPositions = offsetPosition - nbCoupsVides

                        If nbPositions < 1000000 Then
                            offsetPosition = offsetPosition + 1
                            If offsetPosition > UBound(tabPositions) Then
                                ReDim Preserve tabPositions(offsetPosition * 1.1)
                            End If
                            tabPositions(offsetPosition) = chaine

                            If nbPositions Mod 5000 = 0 Then
                                Console.Clear()
                                Console.WriteLine("start position  : " & departEPD & vbCrLf)
                                If tabPositions(0) = "" Then
                                    Console.WriteLine("uci move string : startpos" & vbCrLf)
                                Else
                                    Console.WriteLine("uci move string : " & tabPositions(0) & vbCrLf)
                                End If
                                If nbPositions = 0 Then
                                    Console.WriteLine("Experience rate : 0 moves, avg. D00, 000 counts/move" & vbCrLf)
                                Else
                                    Console.WriteLine("Experience rate : " & Trim(Format(nbPositions, "# ### ##0")) & " moves, avg. D" & Format(moyenne / nbPositions, "00") & ", " & Format(total / nbPositions, "000") & " counts/move" & vbCrLf)
                                End If
                            End If
                        End If
                    End If

                    'reverse ?
                    If modeReverse Then
                        If nbPositions >= 5000 Then
                            indexPosition = indexPosition + 1
                            Exit Do
                        End If
                    End If
                Next
            End If

            indexPosition = indexPosition + 1
        Loop While tabPositions(indexPosition) <> "" And nbCaracteres(tabPositions(indexPosition), " ") < (horizon - 1) And nbPositions < 1000000

        'reverse ?
        If modeReverse Then
            If nbPositions < 1000 Then
                If Len(tabPositions(0)) >= 9 Then
                    'on sauve la position et les stats actuelles
                    departEPDMem = departEPD
                    tabPositionsMem = tabPositions(0)
                    nbPositionsMem = nbPositions
                    totalMem = total
                    moyenneMem = moyenne

                    position = gauche(tabPositions(0), Len(tabPositions(0)) - 5)
                    Array.Clear(tabPositions, 0, tabPositions.Length)
                    GoTo mode_reverse
                End If
            ElseIf tabPositionsMem <> "" Then
                'on restaure la position et les stats précédentes
                departEPD = departEPDMem
                tabPositions(0) = tabPositionsMem
                nbPositions = nbPositionsMem
                total = totalMem
                moyenne = moyenneMem
            End If
        End If

        Console.Clear()
        Console.WriteLine("start position  : " & departEPD & vbCrLf)
        Console.WriteLine("uci move string : " & tabPositions(0) & vbCrLf)

        chaine = "0 000 000 moves, avg. D00, 000 counts/move"
        If nbPositions > 0 Then
            chaine = Format(nbPositions, "0 000 000") & " moves, avg. D" & Format(moyenne / nbPositions, "00") & ", " & Format(total / nbPositions, "000") & " counts/move"
        End If

        nbOuverturesTraitees = nbOuverturesTraitees + 1
        If nbPositions >= 1000 Then
            experienceRate = experienceRate + 1
        End If
        If gauche(chaine, 8) = "0 000 00" Then
            chaine = Replace(chaine, "0 000 00", "........", , 1)
        End If
        If gauche(chaine, 7) = "0 000 0" Then
            chaine = Replace(chaine, "0 000 0", ".......", , 1)
        End If
        If gauche(chaine, 6) = "0 000 " Then
            chaine = Replace(chaine, "0 000 ", "......", , 1)
        End If
        If gauche(chaine, 5) = "0 000" Then
            chaine = Replace(chaine, "0 000", ".....", , 1)
        End If
        If gauche(chaine, 4) = "0 00" Then
            chaine = Replace(chaine, "0 00", "....", , 1)
        End If
        If gauche(chaine, 3) = "0 0" Then
            chaine = Replace(chaine, "0 0", "...", , 1)
        End If
        If gauche(chaine, 2) = "0 " Then
            chaine = Replace(chaine, "0 ", "..", , 1)
        End If
        Console.WriteLine("Experience rate : " & Trim(chaine) & vbCrLf)

        chaine = tabPositions(0) & ";" & chaine

        tabPositions = Nothing

        Return chaine

    End Function

End Module
