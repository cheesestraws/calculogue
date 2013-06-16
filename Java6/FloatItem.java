public class FloatItem extends StackItem
{
        private double value;
        
        public FloatItem(double value) {
            this.value = value;
        }

        public long getIntValue() {
            return Math.round(this.value);
        }
        
        public String getStringValue() {
            return Double.toString(value);
        }
        
        public double getFloatValue() {
            return this.value;
        }
        
}
