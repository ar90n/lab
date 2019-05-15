#include <bits/stdc++.h>
using namespace std;

{% if mod %}
const long long MOD = {{ mod }};
{% endif %}
{% if yes_str %}
const string YES = "{{ yes_str }}";
{% endif %}
{% if no_str %}
const string NO = "{{ no_str }}";
{% endif %}

{% if prediction_success %}
auto solve({{ formal_arguments }}){

}
{% endif %}

int main(){
    {% if prediction_success %}
    {{input_part}}
    auto result = solve({{ actual_arguments }});
    {% else %}
    auto result = 0
    // Failed to predict input format
    {% endif %}
    cout << result << endl;
    return 0;
}
