package document;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.File;
import java.io.IOException;
import java.io.EOFException;

import word.Word;

/**
 * Esta clase representa el documento que se va a procesar. 
 * Conforma el package document.
 */
public class Document {

    private BufferedReader input;    // Documento de entrada.
    private BufferedWriter output;   // Documento de salida.

    /** Constructor de la clase.
     * Abre los archivos de entrada y de salida,
     * colocandolos en los atributos de la clase.
     * @param fname_in    Archivo de entrada.
     * @param fname_out    Archivo de salida.
     * @return Objeto Document.
     * @throws IOException    Si se produce un error al crear el objeto.
     */
    public Document(String fname_in, String fname_out) {
        try {
            File fileIn = new File(fname_in);    // Objeto file input.
            FileReader in = new FileReader(fileIn);    // Objeto lectura file.
            this.input = new BufferedReader(in);    // Lee un texto de una corriente de caracteres de entrada.
            File fileOut = new File(fname_out);    // Objeto file output.
            FileWriter out = new FileWriter(fileOut);    // Objeto escritura file.
            this.output = new BufferedWriter(out);    // Escribe un texto.
        } catch(IOException msg) {
            System.out.println("Error:" + msg.getMessage());
            System.exit(1);    // Salida forzosa del programa.
        };
    }

    /**
     * Cierra los archivos de entrada y de salida.
     * @param void
     * @throws IOException    Si se produce un error al cerrar los archivos.
     */
    public void close() {
        try {
            this.input.close();    // Cierra el documento de entrada.
            this.output.close();    // Cierra el documento de salida.
        } catch(IOException msg) {
            System.out.println("Error:" + msg.getMessage());
            System.exit(1);    // Salida forzosa del programa.
        };
    }

    /**
     * Lee una palabra del archivo de entrada, copiando
     * todo caracter de puntuacion precedente al archivo
     * de salida.
     * @param void
     * @return Word    Palabra que se lee desde el archivo.
     * @throws EOFException    Levanta una excepcion al llegar al EOF.
     * @throws IOException    Si se produce un error al leer el documento.
     */
    public Word getWord() {
        String temp_word = "";    // Palabra a leer del documento.
        try {
            int character;    // Caracter leido del documento.
            int flag = 0;    // Bandera de que se leyo un caracter alfabetico.
            while(true)	{
                try {
                    character = this.input.read();
                    if (character == -1) { throw new EOFException(); }    // Al llegar al EOF, se lo señaliza con una excepción.
                    if(!Character.isLetter(character)) {    // No es un caracter alfabetico.
                        if(flag == 0) {    // No se leyo antes un caracter alfabetico.
                            this.output.write(character);    // Guarda en el archivo de salida el caracter precedente a la palabra.
                        } else {    // Se leyo antes un caracter alfabetico.
                            this.input.reset();    // Se apunta al caracter no alfabetico para guardarlo despues.
                            break;
                        }
                    } else {    // Es un caracter alfabetico.
                        temp_word += (char) character;
                        flag = 1;    // Se leyo un caracter alfabetico.
                        this.input.mark(30);    // Marca hasta donde leyo.
                        if (temp_word.length() > 30) {
                            System.out.println("Error: El documento posee palabras mayores al tamaño maximo permitido.");
                            System.exit(1);    // Salida forzosa del programa por ingresar un documento no valido.
                        }
                    }
                } catch (EOFException msg_eof) {
                    break;    // Tratamiento de la excepcion EOF generada.
                };
            }
        } catch(IOException msg) {
            System.out.println("Error:" + msg.getMessage());
            System.exit(1);    // Salida forzosa del programa.
        };
        Word word = new Word(temp_word);    // Convierte la palabra leida a Word.
        return word;
    }

    /**
     * Escribe la palabra word al archivo de salida.
     * @param word    Palabra a escribir.
     * @throws IOException    Si se produce un error al escribir en el archivo de salida.
     */
    public void putWord(Word word) {
        try {
            String word_write = word.getWord();    // Palabra a guardar.
            this.output.write(word_write);    // Guarda una palabra en el documento de salida.
        } catch(Exception msg) {
            System.out.println("Error:" + msg.getMessage());
            System.exit(1);    // Salida forzosa del programa.
        };
    }
}
