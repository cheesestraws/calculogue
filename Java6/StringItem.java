public class StringItem extends StackItem
{
        String value;
        
        public StringItem(String value) {
            this.value = value;
        }
        
        public long getIntValue() {
            return Long.parseLong(this.value);
        }
        
        public String getStringValue() {
            return this.value;
        }
        
        public double getFloatValue() {
            return Double.parseDouble(this.value);
        }

}
