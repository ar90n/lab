f unction [demix A aaxis daxis] = sepDUET(spec,numsources,ker,p,q,abins,dbins,maxa,maxd)
    class_size = 3;

    if nargin < 9
        maxd = 20;
    end

    if nargin < 8
        maxa = 0.7;
    end

    if nargin < 7
        dbins = 200;
    end

    if nargin < 6
        abins = 200;
    end

    if nargin < 5
        q = 0;
    end

    if nargin < 4
        p = 1;
    end

    if nargin < 3
        ker = 3;
    end

    tf1 = spec.data(:,:,1);
    tf2 = spec.data(:,:,2);

    R21 = (tf2 + eps) ./ (tf1 + eps);

    freq = [1 : size(tf1,1)] * (pi / size(tf1,1));
    fmat = freq(ones(size(tf1,2),1),:)';

    a = abs(R21);
    alpha = a - (1 ./ a);

    delta = -imag(log(R21)) ./fmat ;

    tfweight = (abs(tf1) .* abs(tf2)) .^ p .* abs(fmat) .^ q;

    amask = (abs(alpha) < maxa) & (abs(delta) < maxd)
    alpha_vec = alpha(amask);
    delta_vec = delta(amask);

    tfweight = tfweight(amask);

    alpha_ind = round(1 + (abins - 1) * (alpha_vec + maxa) / (2 * maxa));
    delta_ind = round(1 + (dbins - 1) * (delta_vec + maxd) / (2 * maxd));

    A = full(sparse(alpha_ind,delta_ind,tfweight,abins,dbins));
    A = smooth2DMat(A,ker);
 imagesc([-maxa,maxa],[-maxd,maxd],log(A));
    work_A = A;

    peak_delta = [];
    peak_alpha = [];
    for i = 1 : numsources
        [ind_alpha ind_delta] = find(max(max(work_A)) == work_A)

        peak_alpha(i) = ind_alpha(1);
        peak_delta(i) = ind_delta(1);

        work_A([ind_alpha(1) - class_size:ind_alpha(1) + class_size],[ind_delta(1) - class_size:ind_delta(1) + class_size]) = 0;
    end

    peak_alpha = (peak_alpha - 1) * ((2 * maxa) / (abins - 1)) - maxa;
    peak_delta = (peak_delta - 1) * ((2 * maxd) / (dbins - 1)) - maxd  ;

    peak_a = (peak_alpha + sqrt(peak_alpha .^ 2 + 4)) /2

    minimum_score = Inf * ones(size(tf1));
    best_ind = zeros(size(tf1));
    for i = 1:length(peak_alpha)
        score = abs((peak_a(i) * exp(-sqrt(-1) * fmat * peak_delta(i)) .* tf1) - tf2) .^ 2 / (1 + peak_a(i)^2);

        mask = (score < minimum_score);
        best_ind(mask) = i;
        minimum_score(mask) = score(mask);
    end

    data = [];
    for i = 1:numsources
       mask = (best_ind == i);
       data(:,:,i) = ((tf1 + peak_a(i) * exp(sqrt(-1) * fmat * peak_delta(i)) .* tf2) ./ (1 + peak_a(i) ^2)) .* mask;
    end

    demix = struct('data',{data},...
                   'begin',{spec.begin},...
                   'end',{spec.end},...
                   'org_ch',{spec.org_ch},...
                   'org_all_ch',{spec.org_all_ch},...
                   'file_names',{spec.file_names},...
                   'window',{spec.window},...
                   'step',{spec.step});
    aaxis = linspace(-maxa,maxa,abins);
    daxis = linspace(-maxd,maxd,dbins);
end



