import java.util.LinkedList;
import java.lang.StringBuilder;

public class LFSR347Hack {
    
    private LinkedList<Integer> lfsr;
    private int[] tapPositions;
    
    public LFSR347Hack(int[] initialValues, int[] tapPositions) {
        this.lfsr = new LinkedList<Integer>();
        this.tapPositions = tapPositions;
        for (int n: initialValues) {
            if (n == 0 || n == 1) this.lfsr.addLast(n);
            else throw new IllegalArgumentException();
        }
    }
    
    public String toString() {
        Object[] array = this.lfsr.toArray();
        StringBuilder sb = new StringBuilder();
        for (Object n: array) sb.append(n);
        return sb.toString();
    }
    
    public String step(String operation) {
        
        Integer result = this.lfsr.get(this.tapPositions[0]);
        for (int i = 1; i < this.tapPositions.length; i++) {
            result = xor(result, this.lfsr.get(this.tapPositions[i]));
            if (operation == "XNOR") {
                if (result == 1) result = 0;
                else result = 1;
            }
            else if (operation != "XOR") throw new IllegalArgumentException();
        }
        
        this.lfsr.removeLast();
        this.lfsr.addFirst(result);
        return toString();
    }
    
    public int xor(int a, int b) {
        return a ^ b;
    }
    
    public static void main(String[] args) {
        int[] initialValues = {0, 0, 0, 0, 0, 0, 0, 1};
        int[] tapPositions = {1, 2, 3, 7};
        LFSR347Hack testLFSR = new LFSR347Hack(initialValues, tapPositions);
        for (int i = 1; i < 17; i++) {
            System.out.println(i + " " + testLFSR.step("XOR"));
        }
    }
    
    
}
