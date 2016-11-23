package word;

/**
 * Esta clase representa una palabra sin espacios, ni tabs, ni retornos de
 * linea.
 * Conforma el package word.
 */
public class Word {

    /** Atributo: String. */
    private String word;

    /**
     * Constructor por defecto.
     */
    public Word() {
        this.word = "";
    }

    /**
     * Constructor Word con el string especificado, este no debe contener
     * espacios en blancos.
     *
     * @param w El string sin espacios.
     */
    public Word(String w) {
        this.setWord(w);
    }

    /**
     * Modifica el Word con el string especificado.
     * @param s    String con el cual se modificara Word.
     */
    public void setWord(String s) {
        this.word = s;
    }

    /**
     * Devuelve la palabra en forma de String.
     * @param void
     * @return El string de este word.
     */
    public String getWord() {
        return this.word;
    }

    /**
     * Reescribe el hashcode adecuandolo para word.
     * @param void
     * @return El valor del hash code.
     */
    @Override
    public int hashCode() {
        final int prime = 31;  // Numero primo.
        int result = 1;
        result = prime * result + ((word == "") ? 0 : word.hashCode());
        return result;
    }

    /**
     * Compara dos clases word.
     * @param obj    El objeto a comparar con el actual word.
     * @return true si son iguales los word y false en caso contrario.
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof Word)) {
            return false;
        }
        Word other = (Word) obj;
        return word.equals(other.getWord());
    }
}
