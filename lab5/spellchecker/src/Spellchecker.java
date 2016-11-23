import java.util.Scanner;

import document.Document;
import dictionary.Dictionary;
import dictionary.FileDictionary;
import dictionary.MemDictionary;
import word.Word;

/**
 * Esta clase representa a Spellchecker.
 * Conforma el default package.
 * @version 0.1
 * @author Fernando Copa.
 * @author Ramiro Della Vedova.
 */

public class Spellchecker {

    /**
     * Constructor de la clase.
     */
    public Spellchecker() {}

    /**
     * Consulta al usuario sobre que accion realizar (aceptar, ignorar o reemplazar) con la palabra word.
     * Una vez que el usuario elige, realiza la accion elegida.
     * @param word    Palabra sobre la cual se consulta la accion a realizar.
     * @param main_dict    Diccionario principal.
     * @param main_dict    Diccionario de palabras ignoradas.
     * @return Word    Palabra ya consultada, o nueva palabra.
     * @throws IOException    Si se produce un error.
     */
    public static Word consultUser(Word word, Dictionary main_dict, Dictionary ignored) {
        try {
            char c;    // Caracter para la opcion de accion a realizar.
            Scanner reader_char = new Scanner(System.in).useDelimiter("\\s*");    // Objeto Scanner del caracter.
            do {
                reader_char.reset();    // Se limpia el scanner.
                System.out.println ("Palabra no reconocida: '"+word.getWord()+"'. Aceptar (a) - Ignorar (i) - Reemplazar (r): ");
                c = reader_char.next().charAt(0);    // Se lee el caracter.
                reader_char.reset();    // Se limpia el scanner.
            } while ((c != 'a') && (c != 'i') && (c != 'r'));
            switch (c) {
                case 'a':    // Agrega a diccionario principal.
                    main_dict.add(word);
                    break;
                case 'i':    // Agrega a diccionario de palabras ignoradas.
                    ignored.add(word);
                    break;
                case 'r':    // Reemplaza la palabra.
                    String new_word = "";    // Nueva palabra.
                    Scanner reader_word = new Scanner(System.in);    // Objeto scanner de la nueva palabra.
                    System.out.println ("Ingrese una nueva palabra: ");
                    new_word = reader_word.nextLine();    // Lee la palabra.
                    word.setWord(new_word);    // Pasa la palabra a Word.
                    break;
                default:
                    break;
            }
        } catch(Exception msg) {
            System.out.println("Error:" + msg.getMessage());
            System.exit(1);    // Salida forzosa del programa.
        };
        return word;
    }

    /**
     * Procesa el documento fname_in, palabra por palabra, consultando al usuario sobre la accion a realizar
     * si la palabra no es conocida.
     * @param fname_in    Nombre del archivo a procesar.
     * @param fname_out    Nombre del archivo de salida.
     * @param main_dict    Diccionario principal.
     * @param main_dict    Diccionario de palabras ignoradas.
     * @return void
     * @throws IOException    Si se produce un error.
     */
    public static void processDocument(String fname_in, String fname_out, Dictionary main_dict, Dictionary ignored) {
        try {
            Word current_word = new Word();    // Palabra a procesar.
            Document doc = new Document(fname_in, fname_out);    // Abre documento de entrada y salida.
            while (true) {
                current_word = doc.getWord();    // Lee una palabra del documento de entrada.
                if ((current_word.getWord()) == "") {    // Establece si hay mas palabras par leer.
                    break;
                }
                if (!(main_dict.contains(current_word)) && !(ignored.contains(current_word))) {    // ¿Es una palabra desconocida?
                    current_word = consultUser(current_word, main_dict, ignored);    // Palabra desconocida.
                }
                doc.putWord(current_word);    // Agrega una palabra al documento de salida.
            }
            doc.close();    // Cierra el documento de entrada y de salida.
        } catch(Exception msg) {
            System.out.println("Error:" + msg.getMessage());
            System.exit(1);    // Salida forzosa del programa.
        };
    }

    /**
     * Punto de entrada principal. Abre el diccionario principal, procesa el archivo especificado y
     * guarda los cambios realizados en el diccionario principal.
     */
    public static void main(String[] args) {
        String dict_path;    // Path del diccionario.
        int i = args.length;    // Argumentos de entrada.
        if (i < 1) {    // Verificamos el nro de argumentos.
            System.out.println ("spellchecker.java: nro de argumentos erroneo. Deben ser <documento> [<diccionario>].");
            System.exit(1);
        }
        dict_path = (i >= 2) ? args[1] : "dict.txt";    // No pasa un diccionario por defecto.
        FileDictionary main_dict = new FileDictionary();    // Se crea un diccionario principal.
        main_dict.load(dict_path);    // Se carga el diccionario principal.
        MemDictionary ignored = new MemDictionary();    // Se crea un diccionario de palabras ignoradas.
        processDocument(args[0], "out.txt", main_dict, ignored);    // Se procesa el documento.
        main_dict.save(dict_path);    // Se guarda el diccionario principal.
        main_dict.clear();    // Se destruye el diccionario principal.
        ignored.clear();    // Se destruye el diccionario de palabras ignoradas.
        System.out.println ("El documento '"+args[0]+ "' ha sido procesado. Resultados en out.txt");
    }
}
