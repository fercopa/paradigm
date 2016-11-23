package word;

import java.util.HashSet;
import java.util.Iterator;

    /**
     * Esta clase proporciona una coleccion de words.
     * Conforma el package word.
     */
public class WordSet {

    private HashSet <Word> hs;

    /**
     * Constructor de la coleccion.
     */
    public WordSet() {
        this.hs = new HashSet <Word>();
    }

    /**
     * Agrega un word a la coleccion.
     * @param w    El word a agregar.
     */
    public void add(Word w) {
        // ATENCION: hs.add(obj)  devuelve un booleano.
        this.hs.add(w);
    }

    /**
     * Verifica si el word se encuentra en la coleccion.
     * @param w    El word a verificar si se encuentra en la coleccion.
     * @return true Si el word ya existe en la coleccion, false en caso contrario.
     */
    public boolean contains(Word w) {
        return this.hs.contains(w);
    }

    /**
     * Elimina todos los elementos de la coleccion.
     * @param void
     */
    public void clear() {
        this.hs.clear();
    }

    /**
     * Devuelve un iterator de tipo Word.
     * @param void
     * @return Iterator de la coleccion.
     */
    public Iterator<Word> iterator() {
        return this.hs.iterator();
    }

    /**
     * Devuelve el tamano de la coleccion.
     * @param void
     * @return Tamano entero de la coleccion.
     */
    public int size() {
        return this.hs.size();
    }
}
