public class IntegerItem extends StackItem
{
        private long value;
        
        public IntegerItem(long value) {
            this.value = value;
        }
    
        public long getIntValue() {
            return this.value;
        }
        
        public String getStringValue() {
            return Long.toString(this.value);
        }
        
        public double getFloatValue() {
            return this.value;
        }
        
}
