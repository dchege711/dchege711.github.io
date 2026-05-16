import java.io.BufferedReader;
import java.io.FileReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

public class UnitTestLFSR {
    
    public static void main(String[] args) throws FileNotFoundException, IOException {
        
        String[] testFiles = {
            "test_cases/347_test_1.txt", "test_cases/347_test_2.txt",
            "test_cases/347_test_3.txt", "test_cases/347_test_4.txt" 
        };
        
        for (String fileName: testFiles) {
            FileReader fileReader;
            fileReader = new FileReader(fileName);

            BufferedReader bufferedReader = new BufferedReader(fileReader);
            
            String[] tapPositionsStrings = bufferedReader.readLine().trim().split(",");
            int[] tapPositions = new int[tapPositionsStrings.length];
            for (int i = 0; i < tapPositions.length; i++) {
                tapPositions[i] = Integer.parseInt(tapPositionsStrings[i]);
            }
            
            String[] details = bufferedReader.readLine().split("\\s");
            
            int[] initialValues = new int[details[1].length()];
            for (int i = 0; i < initialValues.length; i++) {
                initialValues[i] = Character.getNumericValue(details[1].charAt(i));
            }
            
            LFSR347Hack testLFSR = new LFSR347Hack(initialValues, tapPositions);
            
            String firstline = bufferedReader.readLine().split(" ")[1];
            assert testLFSR.toString() == firstline: "Error in copying initial values";
            
            String operation = details[0];
            int numberOfIterations = Integer.parseInt(details[2]);
            for (int i = 0; i < numberOfIterations; i++) {
                assert testLFSR.step(operation) == bufferedReader.readLine().trim().split(" ")[1];
            }
            System.out.println("Passed tests for " + fileName);

            bufferedReader.close();
        }
    }
}
