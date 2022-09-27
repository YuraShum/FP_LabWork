import java.util.ArrayList;
import java.util.List;

public class Lab1 {
    public static void main(String[] args) {
        System.out.println(Singleton("Yura"));
        System.out.println(nuull(List.of("")));
        System.out.println(snoc(List.of("Hello", "Lorem"), "Element"));
        System.out.println(length(List.of("first", "second",
                "third","fourth")));


    }
    static List Singleton(Object argument){
        // List.of повертає список з заданою кількістю параметрів.
        return List.of(argument);
    }

    static boolean nuull(List list) {
        // якщо список пустий, то його розмір == 0.
        return list.size() == 0;
    }

    static List<Object> snoc(List<Object> list, Object elem) {
        //Створення списку з колекцією яка знаходиться в list
        List<Object> updatedList = new ArrayList<>(list);
        // додання елемента в кінець списку
        updatedList.add(elem);
        return updatedList;
    }

    static Integer length(List list) {
        // лічильник кількості елементів
        Integer counter = 0;
        for (Object element : list) {
            counter++;
        }
        return counter;
    }
}