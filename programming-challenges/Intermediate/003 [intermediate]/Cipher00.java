/**
 * Takes a piece of text and encrypts it with an alphabetical substitution cipher.
 *
 * Note: Incomplete...
 */

import java.lang.StringBuilder;

public class Cipher00 {

    private int[] cipher;
    private int[] cipherKey;
    private int alphabetLength = 128;

    public Cipher00() {
        this.cipherKey = new int[alphabetLength];
        this.cipher = new int[alphabetLength];
        
        // Each character in ASCII corresponds to a number in [0, 127]
        for (int i = 0; i < alphabetLength; i++) {
            this.cipher[i] = i;
        }

        // Shuffle the array...
        for (int i = 0; i < alphabetLength; i++) {
            int randomIndex = (int)(Math.random() * alphabetLength);
            int temp = this.cipher[randomIndex];
            this.cipher[randomIndex] = this.cipher[i];
            this.cipher[i] = temp;
        }

        // Fill in the key
        for (int i = 0; i < alphabetLength; i++) {
            int shuffledInt = this.cipher[i];
            this.cipherKey[shuffledInt] = i;
        }

    }

    /**
    * Encrypt a string using the cipher.
    */
    public String encrypt(String s) {
        StringBuilder encrypted = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            int intValueOfString = (int)s.charAt(i);
            encrypted.append((char)this.cipher[intValueOfString]);
        }
        return encrypted.toString();
    }

    /**
    * Decrypt a string encrypted by the cipher.
    */
    public String decrypt(String s) {
        StringBuilder decrypted = new StringBuilder();
        for (int i = 0; i < s.length(); i++) {
            int intValueOfString = (int)s.charAt(i);
            decrypted.append((char)this.cipherKey[intValueOfString]);
        }
        return decrypted.toString();
    }

    /**
    * Unit test.
    */
    public static void main(String[] args) {
        Cipher00 cipher = new Cipher00();
        String test = "This is a test";
        String encrypted = cipher.encrypt(test);
        String decrypted = cipher.decrypt(encrypted);
        System.out.println(test + " --> " + encrypted + " --> " + decrypted);
    }

}
