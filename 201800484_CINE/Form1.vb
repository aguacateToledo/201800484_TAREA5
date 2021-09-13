Public Class Form1
    Private Sub button4_Click(sender As Object, e As EventArgs) Handles button4.Click
        If MsgBox("¿DESEA SALIR?", vbQuestion + vbYesNo, "salir") = vbYes Then
            Me.Close()
        End If
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim precio As Double = 0
        Dim descuento1 As Double = 0
        Dim descuento2 As Double = 0
        Dim cantidad As Double = 0
        Dim subTotal As Double = 0
        Dim total As Double = 0
        If IsNumeric(txtCantidad.Text) Then
            cantidad = Val(txtCantidad.Text)
        Else
            MsgBox("No es un numero")
            Return
        End If
        Select Case cbxFunciones.Text
            Case "1"
                Select Case cbxCine.Text
                    Case "A"
                        precio = 30
                        subTotal = precio * cantidad
                    Case "B"
                        precio = 24
                        subTotal = precio * cantidad
                    Case "C"
                        precio = 35
                        subTotal = precio * cantidad
                        descuento1 = subTotal * 0.03
                    Case Else

                End Select
            Case "2"
                Select Case cbxCine.Text
                    Case "A"
                        precio = 35
                        subTotal = precio * cantidad
                        descuento1 = subTotal * 0.015
                    Case "B"
                        precio = 34
                        subTotal = precio * cantidad
                    Case "C"
                        precio = 40
                        subTotal = precio * cantidad
                    Case Else

                End Select

            Case "3"
                Select Case cbxCine.Text
                    Case "A"
                        precio = 40
                        subTotal = precio * cantidad
                    Case "B"
                        precio = 44
                        subTotal = precio * cantidad
                        descuento1 = subTotal * 0.055
                    Case "C"
                        precio = 50
                        subTotal = precio * cantidad
                        descuento1 = subTotal * 0.055
                    Case Else

                End Select
            Case "4"
                Select Case cbxCine.Text
                    Case "A"
                        precio = 40
                        subTotal = precio * cantidad
                    Case "B"
                        precio = 44
                        subTotal = precio * cantidad
                        descuento1 = subTotal * 0.055
                    Case "C"
                        precio = 50
                        subTotal = precio * cantidad
                        descuento1 = subTotal * 0.055
                    Case Else

                End Select
            Case Else

        End Select

        If subTotal > 99 And subTotal < 301 Then
            descuento2 = subTotal * 0.002
        End If

        If subTotal > 300 And subTotal < 501 Then
            descuento2 = subTotal * 0.04
        End If

        If subTotal > 499 And subTotal < 1001 Then
            descuento2 = subTotal * 0.06
        End If

        total = subTotal - descuento1 - descuento2

        subTotales(FILA) = subTotal
        descuentos1(FILA) = descuento1
        descuentos2(FILA) = descuento2
        totales(FILA) = total

        FILA += 1

        printList()
    End Sub

    Sub printList()
        Dim X As Byte
        listPagoInicial.Items.Clear()
        listSaldoAnterior.Items.Clear()
        listServicios.Items.Clear()
        listTarifa.Items.Clear()

        For X = 0 To 10
            If subTotales(X) <> Nothing Then
                listPagoInicial.Items.Add(Str(totales(X)))
                listSaldoAnterior.Items.Add(Str(descuentos2(X)))
                listServicios.Items.Add((subTotales(X)))
                listTarifa.Items.Add((descuentos1(X)))
            Else
                Exit For
            End If
        Next X

    End Sub

    Private Sub btnBodega_Click(sender As Object, e As EventArgs) Handles btnBodega.Click
        Dim J As Byte
        FILA = 0
        For J = 0 To 9
            subTotales(J) = Nothing
            descuentos1(J) = Nothing
            descuentos2(J) = Nothing
            totales(J) = Nothing
        Next J

        listSaldoAnterior.Items.Clear()
        listServicios.Items.Clear()
        listTarifa.Items.Clear()
        listPagoInicial.Items.Clear()
        txtCantidad.Text = 0
    End Sub
End Class
